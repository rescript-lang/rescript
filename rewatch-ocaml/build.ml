exception Error = Project_context.Error
exception Package_error = Project_context.Package_error
exception Build_failure = Compiler_scheduler.Build_failure
exception Parse_failure of string
exception Reported_failure of string
exception Full_rebuild_required

type retained_build = {
  root_config: Config.t;
  build_lock_root: string;
  session: Build_session.t;
}

type attempt_request =
  | One_shot_attempt
  | Initial_watch_attempt
  | Full_watch_attempt
  | Retained_watch_attempt of {
      previous: retained_build;
      changes: Watcher.change list;
    }

(* Build kind controls which persistent markers and diagnostics may be reused.
   Keeping all four states explicit prevents an initial watch build from being
   mistaken for either a disposable command or a retained incremental edit. *)
type compilation_kind = Build_attempt.compilation_kind =
  | One_shot
  | Initial_watch
  | Incremental_watch
  | Full_watch

let compilation_kind = function
  | One_shot_attempt -> One_shot
  | Initial_watch_attempt -> Initial_watch
  | Full_watch_attempt -> Full_watch
  | Retained_watch_attempt _ -> Incremental_watch

let previous_build = function
  | Retained_watch_attempt {previous; changes = _} -> Some previous
  | One_shot_attempt | Initial_watch_attempt | Full_watch_attempt -> None

type incremental_source = {
  package: Package_plan.t;
  source: Build_session.source_reference;
}

let run_scheduled_modules (attempt : Build_attempt.t)
    (prepared : Build_session.prepared) ~compile_step ~namespace_count =
  Compiler_scheduler.run ~poll:attempt.process_poll
    ~warning_state:(Build_session.warning_state attempt.session)
    ~compile_assets:prepared.compile_assets ~build_state:prepared.build_state
    ~candidates:(Build_attempt.take_compile_candidates attempt)
    ~mark_compiled:(fun () -> attempt.compiled <- attempt.compiled + 1)
    ~mark_had_warnings:(fun () -> attempt.had_warnings <- true)
    ~progress:attempt.progress ~compile_step ~namespace_count
    ~verbosity:attempt.verbosity

let run_namespace_jobs (attempt : Build_attempt.t) =
  let jobs = Build_attempt.take_namespace_jobs attempt in
  let started_at = Unix.gettimeofday () in
  Fun.protect
    ~finally:(fun () ->
      attempt.parse_seconds <-
        attempt.parse_seconds +. (Unix.gettimeofday () -. started_at))
    (fun () ->
      let results =
        Process.run_parallel ?poll:attempt.process_poll
          (List.map (fun job -> job.Build_attempt.job) jobs)
      in
      List.iter2
        (fun job result -> job.Build_attempt.finish result)
        jobs results);
  List.length jobs

let write_build_ninja (attempt : Build_attempt.t) =
  (* This empty file is a cache-invalidation marker consumed by editor tooling,
     not a serialized build plan. Only commands that reconstruct the project
     graph call this function. *)
  Build_session.iter_package_plans attempt.session (fun _ package ->
      let path = Filename.concat package.Package_plan.build_dir "build.ninja" in
      let channel = open_out_bin path in
      close_out channel)

let incremental_sources (previous : retained_build) changes =
  (* Reusing the graph is safe only for modifications of already-known source
     paths. Additions, removals, and unknown paths can change module identity or
     package topology, so their caller must reconstruct the build instead. *)
  let included = Hashtbl.create (List.length changes) in
  let sources = ref [] in
  let add normalized_path =
    if not (Hashtbl.mem included normalized_path) then (
      Hashtbl.add included normalized_path ();
      match
        Build_session.find_source_reference previous.session normalized_path
      with
      | Some source ->
        let package =
          match
            Build_session.find_package_plan previous.session source.package_root
          with
          | Some package -> package
          | None -> raise Full_rebuild_required
        in
        sources := {package; source} :: !sources
      | None -> raise Full_rebuild_required)
  in
  List.iter
    (fun (change : Watcher.change) ->
      match change.kind with
      | Watcher.Added | Watcher.Removed -> raise Full_rebuild_required
      | Watcher.Modified ->
        add (Platform.normalize_path_for_comparison change.path))
    changes;
  Build_session.pending_parse_paths previous.session
  |> List.sort String.compare |> List.iter add;
  List.rev !sources

let prepare_incremental previous changes (attempt : Build_attempt.t)
    (prepared : Build_session.prepared) =
  (* A retained edit reparses only the reported paths, then replaces the
     affected modules' dependency edges in memory. This keeps the long-lived
     graph coherent without rediscovering the package tree. *)
  let sources = incremental_sources previous changes in
  let bsc = prepared.compiler_context.bsc_path in
  let started_at = Unix.gettimeofday () in
  List.iter
    (fun source ->
      Build_session.mark_parse_pending attempt.session
        (Platform.normalize_path_for_comparison source.source.absolute_path);
      let key =
        Source.compiler_basename source.package.compile_config
          source.source.module_.Source.name
      in
      (Build_state.find_exn prepared.build_state key).compile_dirty <- true)
    sources;
  sources
  |> List.map (fun source ->
      Source.compiler_basename source.package.compile_config
        source.source.module_.Source.name)
  |> List.sort_uniq String.compare
  |> List.iter (fun name ->
      Output.debug ~verbosity:attempt.verbosity
        ("Generating AST for module: " ^ name));
  let parse_completed =
    Output.Progress.start_grouped attempt.progress ~step:"1/2" ~symbol:"🧱 "
      ~label:"Parsing"
      (List.map
         (fun source ->
           source.package.root ^ "\000" ^ source.source.module_.Source.name)
         sources)
  in
  let results =
    Process.run_parallel_map ?poll:attempt.process_poll
      ~on_complete:parse_completed sources ~job:(fun source ->
        Compiler_process.parse_job ~bsc ~build_dir:source.package.build_dir
          ~config:source.package.compile_config source.source.relative_path)
  in
  let affected_modules = Hashtbl.create (List.length sources) in
  let dependency_updates = ref [] in
  List.iter2
    (fun source result ->
      Hashtbl.replace attempt.preliminary_parses source.source.absolute_path
        (Build_attempt.preliminary_parse result);
      (try
         let modified = (Unix.stat source.source.absolute_path).Unix.st_mtime in
         Hashtbl.replace source.package.source_mtimes
           source.source.relative_path modified
       with Unix.Unix_error _ | Sys_error _ -> raise Full_rebuild_required);
      let key =
        Source.compiler_basename source.package.compile_config
          source.source.module_.Source.name
      in
      let parse_failed = not (Process.succeeded result) in
      let parse_failed =
        match Hashtbl.find_opt affected_modules key with
        | Some (_, _, previous_failed) -> previous_failed || parse_failed
        | None -> parse_failed
      in
      Hashtbl.replace affected_modules key
        (source.package, source.source.module_, parse_failed))
    sources results;
  Hashtbl.iter
    (fun key (package, module_, changed_parse_failed) ->
      if not changed_parse_failed then
        let dependencies path =
          Compiler_process.ast_dependencies
            ~build_dir:package.Package_plan.build_dir (Source.ast_path path)
        in
        let raw_dependencies =
          List.sort_uniq String.compare
            (dependencies module_.Source.implementation
            @
            match module_.Source.interface with
            | None -> []
            | Some path -> dependencies path)
        in
        let node =
          match Build_session.find_global_module attempt.session key with
          | Some node -> node
          | None -> raise Full_rebuild_required
        in
        if node.raw_dependencies <> raw_dependencies then
          dependency_updates :=
            (key, node, raw_dependencies) :: !dependency_updates)
    affected_modules;
  attempt.parse_seconds <- Unix.gettimeofday () -. started_at;
  if !dependency_updates <> [] then (
    Build_session.invalidate_graph_cycle attempt.session;
    List.iter
      (fun (key, node, raw_dependencies) ->
        node.Module_graph.raw_dependencies <- raw_dependencies;
        Build_state.set_dependencies prepared.build_state ~key
          (Module_graph.resolved_dependencies
             ~find_module:(Build_session.find_global_module attempt.session)
             ~find_namespace_maps:
               (Build_session.find_namespace_maps attempt.session)
             node))
      !dependency_updates;
    let cycle =
      Module_graph.find_cycle
        (Build_session.global_module_values attempt.session)
        (Build_session.namespace_map_values attempt.session)
        prepared.build_state
    in
    Build_session.set_graph_cycle attempt.session cycle;
    cycle)
  else
    match Build_session.graph_cycle attempt.session with
    | Build_session.Known_cycle cycle -> cycle
    | Build_session.Unknown_cycle ->
      let cycle =
        Module_graph.find_cycle
          (Build_session.global_module_values attempt.session)
          (Build_session.namespace_map_values attempt.session)
          prepared.build_state
      in
      Build_session.set_graph_cycle attempt.session cycle;
      cycle

let run_with_warning_state ~poll ~warning_state ~request ~no_timing ~verbosity
    ~folder ~prod ~features ~warn_error ~after_build ~filter ~on_state =
  let compilation_kind = compilation_kind request in
  let started_at = Unix.gettimeofday () in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let show_progress = verbosity >= 0 in
  let colors = Output.colors_enabled ~interactive in
  let progress =
    Output.Progress.create ~enabled:(interactive && show_progress) ~color:colors
  in
  let poll () =
    poll ();
    Output.Progress.tick progress
  in
  let process_poll = Some poll in
  let watch = compilation_kind <> One_shot in
  let is_rebuild = compilation_kind = Incremental_watch in
  let should_write_build_ninja =
    match compilation_kind with
    | One_shot | Full_watch -> true
    | Initial_watch | Incremental_watch -> false
  in
  let root = Project_context.canonical_project_root folder in
  let root_config =
    match previous_build request with
    | Some previous -> previous.root_config
    | None -> Config.load_root root
  in
  let build_lock_root = Project_context.workspace_lock_root_for root_config in
  Output.debug ~verbosity
    (Printf.sprintf "Created project context Single project: %S at %S for %S"
       root_config.name root_config.path root_config.root);
  let visited = Hashtbl.create 32 in
  let attempt : Build_attempt.t =
    match previous_build request with
    | Some previous ->
      Build_attempt.create_retained ~session:previous.session ~process_poll
        ~progress ~verbosity
    | None ->
      Build_attempt.create_full ~warning_state ~process_poll ~progress
        ~verbosity
  in
  let parse_messages () = List.rev attempt.parse_messages in
  let parse_output messages =
    messages
    |> List.map (function
        | Build_attempt.Parse_warning output | Build_attempt.Parse_error output
        -> output)
    |> String.concat ""
  in
  let parse_failed = Build_attempt.has_parse_error in
  (* A watch build must retain the attempted state even when later parsing or
     compilation fails, because its successful ASTs and artifact inventory are
     needed to recover incrementally on the next edit. Publish ownership before
     any fallible phase starts. *)
  on_state {root_config; build_lock_root; session = attempt.session};
  let report =
    Build_report.create ~started_at ~interactive ~show_progress ~colors
      ~no_timing ~compilation_kind ~attempt
  in
  let build_ninja_written = ref false in
  let write_build_ninja_once () =
    if should_write_build_ninja && not !build_ninja_written then (
      write_build_ninja attempt;
      build_ninja_written := true)
  in
  let phase_seconds seconds = if no_timing then 0. else seconds in
  let parse_step = if is_rebuild then "1/2" else "2/3" in
  let compile_step = if is_rebuild then "2/2" else "3/3" in
  let report_failure ~compile_seconds output =
    Build_attempt.finalize_logs attempt;
    if attempt.freshness_mode = Build_attempt.Initialize_freshness then
      Source_dirs.write_build ~root_config attempt.session;
    Build_report.report report ~success:false ~compile_seconds;
    prerr_string output;
    prerr_newline ();
    Build_attempt.cleanup_artifacts attempt;
    write_build_ninja_once ();
    raise
      (Reported_failure
         ("Incremental build failed. Error: \027[2K\r  Failed to Compile. "
        ^ "See Errors Above"))
  in
  let report_parse_failure output =
    Build_attempt.finalize_logs attempt;
    Build_report.report_parse_failure report ~output;
    Build_attempt.cleanup_artifacts attempt;
    write_build_ninja_once ();
    raise
      (Reported_failure
         "Incremental build failed. Error: \027[2K\r  Could not parse Source \
          Files")
  in
  let format_cycle cycle (by_key : (string, Module_graph.cycle_node) Hashtbl.t)
      =
    let format_node name =
      match Hashtbl.find_opt by_key name with
      | None -> name
      | Some node -> (
        match node.source_path with
        | Some source_path ->
          let absolute = Filename.concat node.package_root source_path in
          Printf.sprintf "%s (%s)" node.display_name
            (Project_context.relative_or_absolute ~root:root_config.root
               absolute)
        | None ->
          Printf.sprintf "%s (%s namespace map)" node.display_name
            (Project_context.relative_or_absolute ~root:root_config.root
               node.package_root))
    in
    "\nCan't continue... Found a circular dependency in your code:\n"
    ^ (cycle |> List.map format_node |> String.concat "\n → ")
    ^ "\n\
       Possible solutions:\n\
       - Extract shared code into a new module both depend on.\n"
  in
  let execute ~release_build_lock =
    poll ();
    let prepared, cycle =
      match request with
      | Retained_watch_attempt {previous; changes} -> (
        match Build_session.prepared attempt.session with
        | Some prepared ->
          (prepared, prepare_incremental previous changes attempt prepared)
        | None -> raise Full_rebuild_required)
      | One_shot_attempt | Initial_watch_attempt | Full_watch_attempt ->
        let preparation =
          Build_preparation.run ~root_config ~prod ~features ~warn_error ~filter
            ~watch ~attempt ~parse_step ~on_cleanup:(fun seconds ->
              if interactive && show_progress && not is_rebuild then (
                if attempt.compiler_cleaned then
                  print_endline
                    (Output.compiler_cleanup_message ~color:colors ~step:"1/3");
                print_endline
                  (Output.cleanup_message ~color:colors ~step:"1/3"
                     ~cleaned:attempt.cleaned ~total:attempt.previous_asts
                     ~seconds:(phase_seconds seconds))))
        in
        (preparation.prepared, preparation.cycle)
    in
    poll ();
    if attempt.compiler_cleaned && show_progress && not interactive then
      print_endline "Cleaned previous build due to compiler update";
    Option.iter
      (fun (cycle_info : Module_graph.cycle_info) ->
        List.iter
          (fun name -> Hashtbl.replace attempt.blocked_modules name ())
          cycle_info.blocked)
      cycle;
    let root_package =
      match Build_session.find_package_plan attempt.session root with
      | Some package -> package
      | None -> raise (Error ("Package graph was not prepared for " ^ root))
    in
    Package_build.prepare_tree ~seen:visited ~package:root_package ~prepared
      ~watch ~attempt;
    Build_session.mark_freshness_initialized attempt.session;
    let parse_messages = parse_messages () in
    let parse_output = parse_output parse_messages in
    if parse_failed parse_messages then raise (Parse_failure parse_output);
    poll ();
    let namespace_count =
      try run_namespace_jobs attempt
      with Build_failure output ->
        raise (Parse_failure (parse_output ^ output))
    in
    Output.Progress.finish progress;
    if interactive && show_progress then
      print_endline
        (Output.parsing_message ~color:colors ~step:parse_step
           ~count:attempt.parsed
           ~seconds:(phase_seconds attempt.parse_seconds));
    flush stdout;
    prerr_string parse_output;
    flush stderr;
    let compile_started = Unix.gettimeofday () in
    let compile_failure =
      try
        run_scheduled_modules attempt prepared ~compile_step ~namespace_count;
        None
      with Build_failure output -> Some output
    in
    Output.Progress.finish progress;
    let compile_seconds =
      phase_seconds (Unix.gettimeofday () -. compile_started)
    in
    match (compile_failure, cycle) with
    | Some output, _ -> report_failure ~compile_seconds output
    | None, Some cycle_info ->
      let output = format_cycle cycle_info.cycle cycle_info.nodes_by_key in
      cycle_info.cycle
      |> List.filter_map (Hashtbl.find_opt cycle_info.nodes_by_key)
      |> List.map (fun (node : Module_graph.cycle_node) -> node.package_root)
      |> List.sort_uniq String.compare
      |> List.iter (fun package_root -> Compiler_log.append package_root output);
      report_failure ~compile_seconds output
    | None, None ->
      Build_attempt.finalize_logs attempt;
      if attempt.freshness_mode = Build_attempt.Initialize_freshness then
        Source_dirs.write_build ~root_config attempt.session;
      let diagnostics =
        Build_report.print_success_details report ~compile_seconds
      in
      let context = prepared.compiler_context in
      Build_session.publish_compiler_info attempt.session (fun package ->
          let package_context =
            Compiler_info.for_package context
              ~build_root:package.Package_plan.build_owner
              package.compile_config
          in
          Compiler_info.write_package package_context package.config);
      if compilation_kind = One_shot then
        Build_report.report_completion report diagnostics;
      Build_attempt.cleanup_artifacts attempt;
      write_build_ninja_once ();
      release_build_lock ();
      Option.iter
        (fun command -> After_build.run ?poll:process_poll ~root command)
        after_build;
      if compilation_kind <> One_shot then
        Build_report.report_completion report diagnostics
  in
  Build_lock.with_build ~poll build_lock_root
    (fun ~release:release_build_lock ->
      Build_attempt.protect attempt (fun () ->
          try execute ~release_build_lock with
          | Build_failure output -> report_failure ~compile_seconds:0. output
          | Parse_failure output -> report_parse_failure output));
  {root_config; build_lock_root; session = attempt.session}

let run ~poll ~verbosity ~folder ~prod ~features ~warn_error ~after_build
    ~filter ~no_timing =
  try
    run_with_warning_state ~warning_state:(Warning_state.create ()) ~poll
      ~request:One_shot_attempt ~no_timing ~verbosity ~folder ~prod ~features
      ~warn_error ~after_build ~filter ~on_state:(fun _ -> ())
    |> ignore
  with Reported_failure message -> raise (Error message)

let remove_compile_warning_freshness warning_state =
  Warning_state.entries warning_state
  |> List.iter (fun (entry : Warning_state.entry) ->
      let implementation =
        if Filename.check_suffix entry.path ".resi" then
          Filename.chop_suffix entry.path "i"
        else entry.path
      in
      [implementation; implementation ^ "i"]
      |> List.iter (fun source ->
          File_util.remove_file
            (Build_artifacts.published_ast_path
               ~ocaml_dir:(Build_artifacts.lib_path entry.package_root "ocaml")
               source)))

let watch ~verbosity ~folder ~prod ~features ~warn_error ~after_build ~filter
    ~clear_screen =
  let root = Project_context.canonical_project_root folder in
  let warning_state = Warning_state.create () in
  let initial_build = ref true in
  let retained = ref None in
  let force_full_rebuild = ref false in
  let build ~poll ~changes =
    let is_initial = !initial_build in
    initial_build := false;
    try
      let run request =
        let compilation_kind = compilation_kind request in
        let attempted = ref None in
        try
          run_with_warning_state ~poll ~warning_state ~request ~no_timing:false
            ~verbosity ~folder ~prod ~features ~warn_error ~after_build ~filter
            ~on_state:(fun state -> attempted := Some state)
        with exn ->
          (* Failed initial and incremental attempts still own useful parsed
             state. Full reconstruction failures do not, because their graph may
             be only partially discovered. *)
          (match (compilation_kind, !attempted) with
          | (Initial_watch | Incremental_watch), Some state ->
            retained := Some state
          | Full_watch, Some state
            when Option.is_some (Build_session.prepared state.session) ->
            retained := Some state;
            force_full_rebuild := false
          | (One_shot | Full_watch), _ | _, None -> ());
          raise exn
      in
      let next =
        match (!retained, changes, !force_full_rebuild) with
        | Some previous, Some changes, false -> (
          try run (Retained_watch_attempt {previous; changes})
          with Full_rebuild_required ->
            force_full_rebuild := true;
            run Full_watch_attempt)
        | Some _, _, true -> run Full_watch_attempt
        | None, _, _ ->
          run (if is_initial then Initial_watch_attempt else Full_watch_attempt)
        | Some _, None, false -> run Full_watch_attempt
      in
      retained := Some next;
      force_full_rebuild := false;
      Watcher.Succeeded
    with
    | Reported_failure _ -> Watcher.Failed
    | Package_error message
    | Error message
    | Config.Error message
    | Source.Error message
    | Process.Error message ->
      prerr_endline message;
      Watcher.Failed
    | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
      prerr_endline (Printexc.to_string exn);
      Watcher.Failed
  in
  Fun.protect
    (fun () ->
      Watcher.run ~root ~prod ~features ~filter ~clear_screen
        ~show_progress:(verbosity >= 0) ~verbosity ~build)
    ~finally:(fun () ->
      if Warning_state.entries warning_state <> [] then
        match !retained with
        | Some state ->
          Build_lock.with_build state.build_lock_root (fun ~release:_ ->
              remove_compile_warning_freshness warning_state)
        | None -> ())
