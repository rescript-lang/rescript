exception Build_failure of string
exception Module_failed
type cmi_change = Build_state.cmi_change =
  | Cmi_changed
  | Cmi_unchanged
  | Cmi_change_unknown
exception Publication_failure of exn * cmi_change

type publish_result = {stderr: string; cmi_change: cmi_change}
type namespace_task = {
  job: Process.job;
  publish: Process.result -> publish_result;
}
type post_build_task = {output: string; task: Process.task}

type phase =
  | Start
  | Interface of string
  | Implementation of string
  | Post_build of {output: string; remaining: post_build_task list}
  | Done

type publication =
  | Published of publish_result
  | Failed_after_cmi_publication of {error: exn; cmi_change: cmi_change}

type recorded_publication =
  | No_publication
  | Publication_succeeded of string
  | Publication_failed of string

let capture_publication publish =
  try Published (publish ()) with
  | Publication_failure (error, cmi_change) ->
    Failed_after_cmi_publication {error; cmi_change}
  | error ->
    Failed_after_cmi_publication {error; cmi_change = Cmi_change_unknown}

type scheduled_module = {
  key: string;
  dependencies: string list;
  source: Source.module_;
  state: Build_state.module_;
  cmi_path: string;
  publication: publication option Atomic.t;
  prepare: unit -> unit;
  compile: source_kind:Source.source_kind -> string -> Process.job;
  publish:
    source_kind:Source.source_kind -> string -> Process.result -> publish_result;
  record_published_outputs: source_kind:Source.source_kind -> string -> unit;
  post_build: string -> post_build_task list;
  package_root: string;
  is_local: bool;
  mark_warning: string -> unit;
  mutable messages: string list;
  mutable phase: phase;
}

type candidate = {
  key: string;
  state: Build_state.module_;
  warning_paths: string list;
  make: unit -> scheduled_module;
}

type scheduled_item = Module of scheduled_module | Namespace_barrier

let create ~key ~dependencies ~source ~state ~cmi_path ~prepare ~compile
    ~publish ~record_published_outputs ~post_build ~package_root ~is_local
    ~mark_warning =
  {
    key;
    dependencies;
    source;
    state;
    cmi_path;
    publication = Atomic.make None;
    prepare;
    compile;
    publish;
    record_published_outputs;
    post_build;
    package_root;
    is_local;
    mark_warning;
    messages = [];
    phase = Start;
  }

let candidate ~key ~state ~warning_paths ~make =
  {key; state; warning_paths; make}

let candidate_requires_compile candidate = candidate.state.compile_dirty

let run ~poll ~warning_state ~compile_assets ~build_state ~candidates
    ~mark_compiled ~mark_had_warnings ~progress ~compile_step ~namespace_count
    ~verbosity =
  let dirty_propagation = Hashtbl.create 16 in
  let refresh_published_cmi (scheduled : scheduled_module) cmi_change =
    Build_state.record_published_cmi ~dirty_propagation build_state
      ~compile_assets scheduled.state ~path:scheduled.cmi_path cmi_change
  in
  let finish_successful_compile (scheduled : scheduled_module) =
    let cmt_path = Filename.remove_extension scheduled.cmi_path ^ ".cmt" in
    Build_state.record_successful_compile ~compile_assets scheduled.state
      ~cmt_path
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
  let scheduled_keys = Hashtbl.copy universe in
  Hashtbl.iter
    (fun key () ->
      match Build_state.find build_state key with
      | Some state when state.kind = Build_state.Namespace_map ->
        Hashtbl.replace scheduled_keys key ()
      | Some _ | None -> ())
    reached;
  let scheduled_dependencies dependencies =
    List.filter (Hashtbl.mem scheduled_keys) dependencies
  in
  let module_works =
    scheduled_modules
    |> List.map (fun (scheduled : scheduled_module) ->
        Process.
          {
            key = scheduled.key;
            dependencies = scheduled_dependencies scheduled.dependencies;
            value = Module scheduled;
          })
  in
  let namespace_works =
    Hashtbl.to_seq_keys scheduled_keys
    |> Seq.filter_map (fun key ->
        match Build_state.find build_state key with
        | Some state when state.kind = Build_state.Namespace_map ->
          Some
            Process.
              {
                key;
                dependencies = scheduled_dependencies state.dependencies;
                value = Namespace_barrier;
              }
        | Some _ | None -> None)
    |> List.of_seq
  in
  let works = module_works @ namespace_works in
  let record_publication (scheduled : scheduled_module) ~source_kind path =
    let publication = Atomic.exchange scheduled.publication None in
    match publication with
    | Some (Published {stderr; cmi_change}) ->
      refresh_published_cmi scheduled cmi_change;
      scheduled.record_published_outputs ~source_kind path;
      Publication_succeeded stderr
    | Some (Failed_after_cmi_publication {error; cmi_change}) ->
      refresh_published_cmi scheduled cmi_change;
      scheduled.record_published_outputs ~source_kind path;
      Publication_failed (Printexc.to_string error)
    | None -> No_publication
  in
  let record_result (scheduled : scheduled_module) ~source_kind path result =
    let result, publication_error =
      match record_publication scheduled ~source_kind path with
      | Publication_succeeded stderr -> ({result with Process.stderr}, None)
      | Publication_failed message -> (result, Some message)
      | No_publication -> (result, None)
    in
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
      (fun message -> scheduled.messages <- message :: scheduled.messages)
      message;
    Option.is_none message
  in
  let compilation_task (scheduled : scheduled_module) ~source_kind path =
    let job = scheduled.compile ~source_kind path in
    Atomic.set scheduled.publication None;
    Process.task job ~on_result:(fun result ->
        (if Process.succeeded result then
           let publication =
             capture_publication (fun () ->
                 scheduled.publish ~source_kind path result)
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
      scheduled.messages <- message :: scheduled.messages;
      false
  in
  let invalidate_persistent_freshness (scheduled : scheduled_module) =
    let ocaml_dir = Filename.dirname scheduled.cmi_path in
    scheduled.source.Source.implementation
    :: Option.to_list scheduled.source.Source.interface
    |> List.iter (fun source ->
        let path = Build_artifacts.published_ast_path ~ocaml_dir source in
        File_util.remove_file path;
        Compile_assets.refresh_ast compile_assets
          ~source:(Filename.concat scheduled.package_root source)
          ~path)
  in
  let complete_module (scheduled : scheduled_module) =
    scheduled.phase <- Done;
    Output.Progress.advance progress;
    if scheduled.messages <> [] then (
      invalidate_persistent_freshness scheduled;
      raise Module_failed)
    else (
      finish_successful_compile scheduled;
      incr completed_modules)
  in
  let continue_post_build (scheduled : scheduled_module) tasks =
    match tasks with
    | [] ->
      complete_module scheduled;
      None
    | {output; task} :: remaining ->
      scheduled.phase <- Post_build {output; remaining};
      Some task
  in
  let reconcile_unconsumed_publications () =
    List.iter
      (fun (scheduled : scheduled_module) ->
        let source =
          match scheduled.phase with
          | Interface path -> Some (Source.Interface, path)
          | Implementation path -> Some (Source.Implementation, path)
          | Start | Post_build _ | Done -> None
        in
        Option.iter
          (fun (source_kind, path) ->
            match record_publication scheduled ~source_kind path with
            | Publication_failed message ->
              scheduled.messages <- message :: scheduled.messages
            | No_publication | Publication_succeeded _ -> ())
          source)
      scheduled_modules
  in
  let scheduler_failed =
    try
      Process.run_dependency_graph ?poll
        works
        (* A module failure stops new work while subprocesses that already own
           resources finish, matching the command-level failure policy. *)
        ~on_failure:(function
          | Module_failed -> Process.Stop_new_work
          | _ -> Process.Abort_immediately)
        ~next:(fun item result ->
          match item with
          | Namespace_barrier -> (
            match result with
            | None -> None
            | Some _ ->
              raise
                (Project_context.Error
                   "namespace scheduler barrier produced a process result"))
          | Module scheduled -> (
            match (result, scheduled.phase) with
            | None, Start ->
              if scheduled.state.compile_dirty then (
                mark_compiled ();
                scheduled.prepare ();
                match scheduled.source.Source.interface with
                | Some path ->
                  Output.Progress.debug progress ~verbosity
                    ("Compiling interface file: " ^ scheduled.key);
                  scheduled.phase <- Interface path;
                  Some
                    (compilation_task scheduled ~source_kind:Source.Interface
                       path)
                | None ->
                  let path = scheduled.source.Source.implementation in
                  Output.Progress.debug progress ~verbosity
                    ("Compiling file: " ^ scheduled.key);
                  scheduled.phase <- Implementation path;
                  Some
                    (compilation_task scheduled
                       ~source_kind:Source.Implementation path))
              else (
                scheduled.phase <- Done;
                incr completed_modules;
                Output.Progress.advance progress;
                None)
            | Some result, Interface path ->
              ignore
                (record_result scheduled ~source_kind:Source.Interface path
                   result);
              let path = scheduled.source.Source.implementation in
              Output.Progress.debug progress ~verbosity
                ("Compiling file: " ^ scheduled.key);
              scheduled.phase <- Implementation path;
              Some
                (compilation_task scheduled ~source_kind:Source.Implementation
                   path)
            | Some result, Implementation path ->
              if
                record_result scheduled ~source_kind:Source.Implementation path
                  result
              then continue_post_build scheduled (scheduled.post_build path)
              else (
                complete_module scheduled;
                None)
            | Some result, Post_build {output; remaining} ->
              if record_post_build_result scheduled output result then
                continue_post_build scheduled remaining
              else (
                complete_module scheduled;
                None)
            | None, (Interface _ | Implementation _ | Post_build _ | Done)
            | Some _, (Start | Done) ->
              raise (Project_context.Error "invalid compiler scheduler state")));
      false
    with exn -> (
      reconcile_unconsumed_publications ();
      match exn with
      | Module_failed -> true
      | _ -> raise exn)
  in
  Output.Progress.finish progress;
  Output.trace ~verbosity
    (Printf.sprintf "Compiled %d out of %d in the universe" !completed_modules
       (List.length scheduled_modules));
  let failures =
    scheduled_modules
    |> List.sort (fun (first : scheduled_module) second ->
        String.compare first.key second.key)
    |> List.concat_map (fun scheduled ->
        scheduled.messages |> List.rev
        |> List.map (fun output -> (scheduled, output)))
  in
  Warning_state.entries warning_state
  |> List.iter (fun entry ->
      Compiler_log.append entry.Warning_state.package_root entry.output);
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
