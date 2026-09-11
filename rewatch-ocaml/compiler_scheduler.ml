exception Build_failure of string
exception Module_failed

type phase =
  | Start
  | Interface of string
  | Implementation of string
  | Post_build of string * (string * Process.task) list
  | Done

type publication =
  | Published of {stderr: string; cmi_digest: Digest.t option}
  | Failed_after_cmi_publication of {error: exn; cmi_digest: Digest.t option}

type scheduled_module = {
  key: string;
  dependencies: string list;
  source: Source.module_;
  state: Build_state.module_;
  cmi_path: string;
  mutable cmi_digest_before: Digest.t option;
  publication: publication option Atomic.t;
  prepare: unit -> unit;
  compile: is_interface:bool -> string -> Process.job;
  publish: is_interface:bool -> string -> Process.result -> string;
  record_published_outputs: is_interface:bool -> string -> unit;
  post_build: string -> (string * Process.task) list;
  package_root: string;
  is_local: bool;
  mark_warning: string -> unit;
  messages: string list ref;
  phase: phase ref;
}

type candidate = {
  key: string;
  state: Build_state.module_;
  warning_paths: string list;
  make: unit -> scheduled_module;
}

let create ~key ~dependencies ~source ~state ~cmi_path ~prepare ~compile
    ~publish ~record_published_outputs ~post_build ~package_root ~is_local
    ~mark_warning =
  {
    key;
    dependencies;
    source;
    state;
    cmi_path;
    cmi_digest_before = None;
    publication = Atomic.make None;
    prepare;
    compile;
    publish;
    record_published_outputs;
    post_build;
    package_root;
    is_local;
    mark_warning;
    messages = ref [];
    phase = ref Start;
  }

let candidate ~key ~state ~warning_paths ~make =
  {key; state; warning_paths; make}

let candidate_requires_compile candidate = candidate.state.compile_dirty

let file_digest path =
  try Some (Digest.file path) with Sys_error _ | Unix.Unix_error _ -> None

let run ~poll ~warning_state ~blocked_modules ~compile_assets ~build_state
    ~candidates ~mark_compiled ~mark_had_warnings ~progress ~compile_step
    ~namespace_count ~verbosity =
  let refresh_published_cmi (scheduled : scheduled_module) cmi_digest_after =
    (* Only a changed interface invalidates reverse dependents. Comparing bytes
       avoids timestamp races and skips unnecessary downstream compilation. *)
    let cmi_changed =
      match (scheduled.cmi_digest_before, cmi_digest_after) with
      | Some before, Some after -> before <> after
      | _ -> true
    in
    Compile_assets.refresh_cmi compile_assets ~key:scheduled.key
      ~path:scheduled.cmi_path;
    scheduled.state.last_compiled_cmi <-
      Compile_assets.cmi compile_assets scheduled.key
      |> Option.map (fun entry -> entry.Compile_assets.modified);
    scheduled.cmi_digest_before <- cmi_digest_after;
    if cmi_changed then
      Build_state.mark_dependents_compile_dirty build_state scheduled.state
        ~is_blocked:(Hashtbl.mem blocked_modules)
  in
  let finish_successful_compile (scheduled : scheduled_module) =
    let cmt_path = Filename.remove_extension scheduled.cmi_path ^ ".cmt" in
    Compile_assets.refresh_cmt compile_assets ~key:scheduled.key ~path:cmt_path;
    scheduled.state.last_compiled_cmt <-
      Compile_assets.cmt compile_assets scheduled.key
      |> Option.map (fun entry -> entry.Compile_assets.modified);
    scheduled.state.compile_dirty <- false
  in
  let warning_paths =
    candidates |> List.concat_map (fun candidate -> candidate.warning_paths)
  in
  Warning_state.retain_paths warning_state warning_paths;
  if Output.trace_enabled verbosity then
    candidates
    |> List.filter candidate_requires_compile
    |> List.sort (fun first second -> String.compare first.key second.key)
    |> List.iter (fun candidate ->
        Printf.printf "compile dirty: %s\n%!" candidate.key);
  (* The scheduler only needs dirty modules and their transitive dependents.
     Dependencies outside that universe already have usable artifacts, while
     keeping every module in the subprocess graph makes small edits scale with
     the whole project. *)
  let candidate_by_key = Hashtbl.create (List.length candidates) in
  List.iter
    (fun candidate -> Hashtbl.replace candidate_by_key candidate.key candidate)
    candidates;
  let universe = Hashtbl.create (List.length candidates) in
  let reached = Hashtbl.create (List.length candidates) in
  let pending = Queue.create () in
  let add_to_closure key =
    if not (Hashtbl.mem reached key) then (
      Hashtbl.add reached key ();
      if Hashtbl.mem candidate_by_key key then Hashtbl.add universe key ();
      Queue.add key pending)
  in
  candidates
  |> List.iter (fun candidate ->
      if candidate.state.compile_dirty then add_to_closure candidate.key);
  while not (Queue.is_empty pending) do
    let key = Queue.take pending in
    let state = Build_state.find_exn build_state key in
    Build_state.String_set.iter add_to_closure state.dependents
  done;
  let scheduled_modules =
    candidates
    |> List.filter (fun candidate -> Hashtbl.mem universe candidate.key)
    |> List.map (fun candidate -> candidate.make ())
  in
  Output.Progress.start progress ~step:compile_step ~symbol:"🤺 "
    ~label:"Compiling"
    ~total:(namespace_count + List.length scheduled_modules);
  for _ = 1 to namespace_count do
    Output.Progress.advance progress
  done;
  let completed_modules = ref 0 in
  let scheduled_dependencies dependencies =
    let visited = Hashtbl.create 4 in
    let rec expand dependency =
      if Hashtbl.mem visited dependency then []
      else (
        Hashtbl.add visited dependency ();
        if Hashtbl.mem universe dependency then [dependency]
        else
          match Build_state.find build_state dependency with
          | Some state when state.kind = Build_state.Namespace_map ->
            List.concat_map expand state.dependencies
          | Some _ | None -> [])
    in
    dependencies |> List.concat_map expand |> List.sort_uniq String.compare
  in
  let works =
    scheduled_modules
    |> List.map (fun (scheduled : scheduled_module) ->
        Process.
          {
            key = scheduled.key;
            dependencies = scheduled_dependencies scheduled.dependencies;
            value = scheduled;
          })
  in
  let record_result (scheduled : scheduled_module) ~is_interface path result =
    let publication = Atomic.exchange scheduled.publication None in
    let result, publication_error, cmi_digest =
      match publication with
      | Some (Published {stderr; cmi_digest}) ->
        ({result with Process.stderr}, None, Some cmi_digest)
      | Some (Failed_after_cmi_publication {error; cmi_digest}) ->
        (result, Some (Printexc.to_string error), Some cmi_digest)
      | None -> (result, None, None)
    in
    Option.iter
      (fun cmi_digest ->
        refresh_published_cmi scheduled cmi_digest;
        scheduled.record_published_outputs ~is_interface path)
      cmi_digest;
    let message =
      match publication_error with
      | Some message ->
        Warning_state.remove warning_state ~package_root:scheduled.package_root
          ~path;
        Some message
      | None ->
        if Process.succeeded result then (
          match result.Process.stderr with
          | "" ->
            Warning_state.remove warning_state
              ~package_root:scheduled.package_root ~path;
            None
          | warning ->
            mark_had_warnings ();
            Warning_state.set warning_state ~module_name:scheduled.key
              ~package_root:scheduled.package_root ~path ~output:warning;
            if scheduled.is_local then scheduled.mark_warning path;
            None)
        else (
          Warning_state.remove warning_state
            ~package_root:scheduled.package_root ~path;
          Some (result.Process.stderr ^ result.Process.stdout))
    in
    Option.iter
      (fun message -> scheduled.messages := message :: !(scheduled.messages))
      message;
    Option.is_none message
  in
  let compilation_task (scheduled : scheduled_module) ~is_interface path =
    let job = scheduled.compile ~is_interface path in
    Atomic.set scheduled.publication None;
    Process.task job ~on_result:(fun result ->
        (if Process.succeeded result then
           let publication =
             try
               let stderr = scheduled.publish ~is_interface path result in
               Published {stderr; cmi_digest = file_digest scheduled.cmi_path}
             with error ->
               Failed_after_cmi_publication
                 {error; cmi_digest = file_digest scheduled.cmi_path}
           in
           Atomic.set scheduled.publication (Some publication));
        result)
  in
  let record_post_build_result (scheduled : scheduled_module) output result =
    if Process.succeeded result then (
      if result.Process.stdout <> "" then print_string result.stdout;
      if result.stderr <> "" then prerr_string result.stderr;
      true)
    else
      let captured = result.stderr ^ result.stdout in
      let message =
        Printf.sprintf "js-post-build command failed for %s%s" output
          (if captured = "" then "" else "\n" ^ captured)
      in
      scheduled.messages := message :: !(scheduled.messages);
      false
  in
  let complete_module (scheduled : scheduled_module) =
    scheduled.phase := Done;
    Output.Progress.advance progress;
    if !(scheduled.messages) <> [] then raise Module_failed
    else (
      finish_successful_compile scheduled;
      incr completed_modules)
  in
  let continue_post_build (scheduled : scheduled_module) tasks =
    match tasks with
    | [] ->
      complete_module scheduled;
      None
    | (output, task) :: remaining ->
      scheduled.phase := Post_build (output, remaining);
      Some task
  in
  let scheduler_failed =
    try
      Process.run_dependency_graph ?poll
        works
        (* A module failure blocks its dependents, while unrelated ready
               work is drained so all independent diagnostics are retained. *)
        ~is_fatal:(function
          | Module_failed -> false
          | _ -> true)
        ~next:(fun scheduled result ->
          match (result, !(scheduled.phase)) with
          | None, Start ->
            if scheduled.state.compile_dirty then (
              mark_compiled ();
              scheduled.prepare ();
              scheduled.cmi_digest_before <- file_digest scheduled.cmi_path;
              match scheduled.source.Source.interface with
              | Some path ->
                Output.Progress.debug progress ~verbosity
                  ("Compiling interface file: " ^ scheduled.key);
                scheduled.phase := Interface path;
                Some (compilation_task scheduled ~is_interface:true path)
              | None ->
                let path = scheduled.source.Source.implementation in
                Output.Progress.debug progress ~verbosity
                  ("Compiling file: " ^ scheduled.key);
                scheduled.phase := Implementation path;
                Some (compilation_task scheduled ~is_interface:false path))
            else (
              scheduled.phase := Done;
              incr completed_modules;
              Output.Progress.advance progress;
              None)
          | Some result, Interface path ->
            ignore (record_result scheduled ~is_interface:true path result);
            let path = scheduled.source.Source.implementation in
            Output.Progress.debug progress ~verbosity
              ("Compiling file: " ^ scheduled.key);
            scheduled.phase := Implementation path;
            Some (compilation_task scheduled ~is_interface:false path)
          | Some result, Implementation path ->
            if record_result scheduled ~is_interface:false path result then
              continue_post_build scheduled (scheduled.post_build path)
            else (
              complete_module scheduled;
              None)
          | Some result, Post_build (output, remaining) ->
            if record_post_build_result scheduled output result then
              continue_post_build scheduled remaining
            else (
              complete_module scheduled;
              None)
          | None, (Interface _ | Implementation _ | Post_build _ | Done)
          | Some _, (Start | Done) ->
            raise (Project_context.Error "invalid compiler scheduler state"));
      false
    with Module_failed -> true
  in
  Output.Progress.finish progress;
  Output.trace ~verbosity
    (Printf.sprintf "Compiled %d out of %d in the universe" !completed_modules
       (List.length scheduled_modules));
  let failures = ref [] in
  scheduled_modules
  |> List.sort (fun (first : scheduled_module) second ->
      String.compare first.key second.key)
  |> List.iter (fun (scheduled : scheduled_module) ->
      !(scheduled.messages) |> List.rev
      |> List.iter (fun output -> failures := (scheduled, output) :: !failures));
  Warning_state.entries warning_state
  |> List.iter (fun entry ->
      Compiler_log.append entry.Warning_state.package_root entry.output);
  let failures = List.rev !failures in
  List.iter
    (fun ((scheduled : scheduled_module), output) ->
      Compiler_log.append scheduled.package_root output)
    failures;
  match (failures, scheduler_failed) with
  | [], false -> ()
  | [], true ->
    raise
      (Project_context.Error "compiler scheduler stopped without a diagnostic")
  | failures, _ ->
    failures |> List.map snd |> String.concat "" |> fun output ->
    raise (Build_failure output)
