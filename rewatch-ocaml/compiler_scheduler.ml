exception Build_failure of string
exception Module_failed
type cmi_change = Build_state.cmi_change =
  | Cmi_changed
  | Cmi_unchanged
  | Cmi_change_unknown
exception Publication_failure of exn * cmi_change

type publish_result = {stderr: string; cmi_change: cmi_change}
type namespace_task = {
  task: Process.task;
  publish: Process.result -> publish_result;
}
type post_build_task = {output: string; task: Process.task}

type compile_hooks = {
  gate: Speculation_gate.t option;
  on_interface_computed: (string -> string -> unit) option;
  cmi_redirects: (string * string) list;
  cmi_directories: string list;
}

let no_compile_hooks =
  {
    gate = None;
    on_interface_computed = None;
    cmi_redirects = [];
    cmi_directories = [];
  }

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
  compile:
    hooks:compile_hooks ->
    source_kind:Source.source_kind ->
    string ->
    Process.task;
  publish:
    source_kind:Source.source_kind -> string -> Process.result -> publish_result;
  record_published_outputs: source_kind:Source.source_kind -> string -> unit;
  post_build: string -> post_build_task list;
  package_root: string;
  is_local: bool;
  mark_warning: string -> unit;
  mutable messages: string list;
  mutable phase: phase;
  mutable speculation: Speculation_gate.t option;
  mutable speculation_abandoned: bool;
  mutable attempt: int;
      (* Identifies the latest compile request, so a message from an
         abandoned speculation is not taken for one from its successor. *)
}

type candidate = {
  key: string;
  state: Build_state.module_;
  warning_paths: string list;
  make: unit -> scheduled_module;
}

type scheduled_item = Module of scheduled_module | Namespace_barrier of string

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
    speculation = None;
    speculation_abandoned = false;
    attempt = 0;
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
  Output.Progress.start progress ~step:compile_step
    ~symbol:Platform.build_symbol ~label:"Compiling"
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
                value = Namespace_barrier key;
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
  let publish_compilation (scheduled : scheduled_module) ~source_kind path
      result =
    (if Process.succeeded result then
       let publication =
         capture_publication (fun () ->
             scheduled.publish ~source_kind path result)
       in
       Atomic.set scheduled.publication (Some publication));
    result
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
  (* The worker that compiled a module also publishes its artifacts, which
     only touches that module's files: dependents are scheduled after its result
     is recorded. Build-state updates stay on this scheduler domain. A stop
     cancels a compile before publication starts, never during it. *)
  let compilation_task ?(hooks = no_compile_hooks)
      (scheduled : scheduled_module) ~source_kind path =
    let task = scheduled.compile ~hooks ~source_kind path in
    Atomic.set scheduled.publication None;
    Process.map_result task (publish_compilation scheduled ~source_kind path)
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
  (* Pipelining. A module's dependents may start type checking as soon as its
     interface is final: after its .cmi is written, or after its interface
     file compiled. Such a speculative request reads not yet published .cmi
     files through redirects and waits before writing anything until the
     module is ready in the dependency graph, so its artifacts, diagnostics and
     effects are those of a request started then. Codegen therefore always
     sees the .cmj of completed dependencies. *)
  let max_jobs = Compiler_execution_mode.configured_count () in
  let mailbox = Process.create_mailbox () in
  (* Test-only record of speculation decisions, as REWATCH_COMPILER_CALL_LOG
     records compiler calls. Written on this scheduling domain. *)
  let speculation_log =
    match Sys.getenv_opt "REWATCH_SPECULATION_LOG" with
    | None -> fun _ _ -> ()
    | Some path ->
      fun event key ->
        let channel =
          open_out_gen [Open_creat; Open_append; Open_binary] 0o644 path
        in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () -> Printf.fprintf channel "%s\t%s\n" event key)
  in
  let module_by_key = Hashtbl.create (List.length scheduled_modules) in
  List.iter
    (fun (scheduled : scheduled_module) ->
      Hashtbl.replace module_by_key scheduled.key scheduled)
    scheduled_modules;
  let dependents_by_key = Hashtbl.create (List.length works) in
  works
  |> List.iter (fun (work : _ Process.work) ->
      List.iter
        (fun dependency ->
          Hashtbl.replace dependents_by_key dependency
            (work.key
            :: Option.value ~default:[]
                 (Hashtbl.find_opt dependents_by_key dependency)))
        work.dependencies);
  let dependencies_by_key = Hashtbl.create (List.length works) in
  List.iter
    (fun (work : _ Process.work) ->
      Hashtbl.replace dependencies_by_key work.key work.dependencies)
    works;
  (* Modules (and namespace barriers) whose work completed successfully. *)
  let completed = Hashtbl.create (List.length works) in
  (* Running modules whose interface dependents may type check against. *)
  let interface_ready = Hashtbl.create 16 in
  let to_check = Hashtbl.create 16 in
  let eligible = Hashtbl.create 16 in
  let check_dependents key =
    Hashtbl.find_opt dependents_by_key key
    |> Option.iter
         (List.iter (fun dependent -> Hashtbl.replace to_check dependent ()))
  in
  let mark_completed key =
    Hashtbl.remove interface_ready key;
    Hashtbl.replace completed key ();
    check_dependents key
  in
  let has_post_build (scheduled : scheduled_module) =
    scheduled.post_build scheduled.source.Source.implementation <> []
  in
  let mark_interface_ready (scheduled : scheduled_module) ready =
    (* A dependent waits for this module's completion while holding a job
       slot, so only modules whose remaining work is already queued may
       release dependents: post-build tasks are queued later. *)
    if not (has_post_build scheduled) then (
      Hashtbl.replace interface_ready scheduled.key ready;
      check_dependents scheduled.key)
  in
  let speculation_eligible (scheduled : scheduled_module) =
    scheduled.phase = Start
    && Option.is_none scheduled.speculation
    && (not scheduled.speculation_abandoned)
    &&
    let dependencies =
      Option.value ~default:[]
        (Hashtbl.find_opt dependencies_by_key scheduled.key)
    in
    let waiting =
      List.filter (fun key -> not (Hashtbl.mem completed key)) dependencies
    in
    waiting <> []
    && List.for_all (Hashtbl.mem interface_ready) waiting
    && (scheduled.state.compile_dirty
       || List.exists
            (fun key ->
              match Hashtbl.find interface_ready key with
              | _, Cmi_unchanged -> false
              | _, (Cmi_changed | Cmi_change_unknown) -> true)
            waiting)
  in
  let speculation_candidates () =
    Hashtbl.iter (fun key () -> Hashtbl.replace eligible key ()) to_check;
    Hashtbl.reset to_check;
    let keys = Hashtbl.fold (fun key () keys -> key :: keys) eligible [] in
    List.filter
      (fun key ->
        match Hashtbl.find_opt module_by_key key with
        | Some scheduled when speculation_eligible scheduled -> true
        | Some _ | None ->
          Hashtbl.remove eligible key;
          false)
      keys
  in
  let interface_hook (scheduled : scheduled_module) =
    scheduled.attempt <- scheduled.attempt + 1;
    let attempt = scheduled.attempt in
    (* Nothing can speculate on one domain, or without scheduled dependents. *)
    if
      max_jobs < 2
      || (not (Hashtbl.mem dependents_by_key scheduled.key))
      || has_post_build scheduled
    then None
    else
      Some
        (fun built_cmi bytes ->
          (* On the worker, before the request writes the .cmi. A dependent
             looks the file up by its module name first, which only names this
             file on a case-insensitive file system if the spellings match, so
             a lowercase file name is not redirected. *)
          let basename = Filename.basename scheduled.cmi_path in
          if
            Filename.basename built_cmi = basename
            && String.capitalize_ascii basename = basename
          then
            (* Read past the build file store: an abandoned request can
               still run this while its successor publishes the file. *)
            let change =
              match File_util.read_file scheduled.cmi_path with
              | published when published = bytes -> Cmi_unchanged
              | _ -> Cmi_changed
              | exception (Unix.Unix_error _ | Sys_error _) -> Cmi_changed
            in
            Process.post mailbox (fun () ->
                if scheduled.attempt = attempt then
                  match (scheduled.phase, scheduled.speculation) with
                  | Implementation _, None when scheduled.messages = [] ->
                    mark_interface_ready scheduled (Some bytes, change)
                  | Start, Some _ ->
                    (* A speculation: its dependents are abandoned with it. *)
                    mark_interface_ready scheduled (Some bytes, change)
                  | ( ( Start | Interface _ | Implementation _ | Post_build _
                      | Done ),
                      _ ) ->
                    ()))
  in
  let first_source (scheduled : scheduled_module) =
    match scheduled.source.Source.interface with
    | Some path -> (Source.Interface, path)
    | None -> (Source.Implementation, scheduled.source.Source.implementation)
  in
  let cmi_directories = lazy (Compile_assets.directories compile_assets) in
  let start_speculation (scheduled : scheduled_module) =
    Hashtbl.remove eligible scheduled.key;
    if not (speculation_eligible scheduled) then None
    else
      let gate = Speculation_gate.create () in
      let cmi_redirects =
        Hashtbl.fold
          (fun key (bytes, _) redirects ->
            match (bytes, Hashtbl.find_opt module_by_key key) with
            | Some bytes, Some (dependency : scheduled_module) ->
              (dependency.cmi_path, bytes) :: redirects
            | _ -> redirects)
          interface_ready []
      in
      let source_kind, path = first_source scheduled in
      let on_interface_computed =
        match source_kind with
        | Source.Implementation -> interface_hook scheduled
        | Source.Interface -> None
      in
      let task =
        compilation_task scheduled
          ~hooks:
            {
              gate = Some gate;
              on_interface_computed;
              cmi_redirects;
              cmi_directories = Lazy.force cmi_directories;
            }
          ~source_kind path
      in
      scheduled.speculation <- Some gate;
      speculation_log "start" scheduled.key;
      Some task
  in
  let abandon_speculation (scheduled : scheduled_module) =
    speculation_log "abandon" scheduled.key;
    Option.iter Speculation_gate.abandon scheduled.speculation;
    scheduled.speculation <- None;
    scheduled.speculation_abandoned <- true;
    (* Dependents read this interface only through the speculation. *)
    if scheduled.phase = Start then Hashtbl.remove interface_ready scheduled.key
  in
  let speculation =
    Process.
      {
        mailbox;
        candidates = speculation_candidates;
        start =
          (function
          | Module scheduled -> start_speculation scheduled
          | Namespace_barrier _ -> None);
        abandon =
          (function
          | Module scheduled -> abandon_speculation scheduled
          | Namespace_barrier _ -> ());
      }
  in
  let start_compilation (scheduled : scheduled_module) ~source_kind path =
    match scheduled.speculation with
    | Some gate ->
      speculation_log "adopt" scheduled.key;
      scheduled.speculation <- None;
      Speculation_gate.open_ gate;
      Process.adopt_speculation
    | None ->
      let on_interface_computed =
        match source_kind with
        | Source.Implementation -> interface_hook scheduled
        | Source.Interface -> None
      in
      compilation_task scheduled
        ~hooks:{no_compile_hooks with on_interface_computed}
        ~source_kind path
  in
  let complete_module (scheduled : scheduled_module) =
    scheduled.phase <- Done;
    Output.Progress.advance progress;
    if scheduled.messages <> [] then (
      Hashtbl.remove interface_ready scheduled.key;
      invalidate_persistent_freshness scheduled;
      raise Module_failed)
    else (
      finish_successful_compile scheduled;
      mark_completed scheduled.key;
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
        let attempt_is_incomplete =
          match scheduled.phase with
          | Interface _ | Implementation _ | Post_build _ -> true
          | Start | Done -> false
        in
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
          source;
        if attempt_is_incomplete then invalidate_persistent_freshness scheduled)
      scheduled_modules
  in
  let () =
    try
      Process.run_dependency_graph ~max_jobs ?poll ~speculation
        works
        (* Finish ready independent work after a module failure; dependents of
           the failed module remain blocked. Interruptions still abort. *)
        ~on_failure:(function
          | Module_failed -> Process.Continue_independent_work
          | _ -> Process.Abort_immediately)
        ~next:(fun item result ->
          match item with
          | Namespace_barrier key ->
            mark_completed key;
            None
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
                    (start_compilation scheduled ~source_kind:Source.Interface
                       path)
                | None ->
                  let path = scheduled.source.Source.implementation in
                  Output.Progress.debug progress ~verbosity
                    ("Compiling file: " ^ scheduled.key);
                  scheduled.phase <- Implementation path;
                  Some
                    (start_compilation scheduled
                       ~source_kind:Source.Implementation path))
              else (
                scheduled.phase <- Done;
                mark_completed scheduled.key;
                incr completed_modules;
                Output.Progress.advance progress;
                None)
            | Some result, Interface path ->
              let interface_succeeded =
                record_result scheduled ~source_kind:Source.Interface path
                  result
              in
              let path = scheduled.source.Source.implementation in
              Output.Progress.debug progress ~verbosity
                ("Compiling file: " ^ scheduled.key);
              scheduled.phase <- Implementation path;
              let task =
                compilation_task scheduled ~source_kind:Source.Implementation
                  path
              in
              (* The published .cmi is final; the implementation is queued
                 before any dependent can start. *)
              if interface_succeeded then
                mark_interface_ready scheduled (None, Cmi_unchanged);
              Some task
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
              raise (Project_context.Error "invalid compiler scheduler state")))
    with exn -> (
      reconcile_unconsumed_publications ();
      match exn with
      | Module_failed -> ()
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
  match failures with
  | [] -> ()
  | failures ->
    failures |> List.map snd |> String.concat "" |> fun output ->
    raise (Build_failure output)
