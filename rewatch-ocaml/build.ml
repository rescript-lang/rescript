exception Error = Project_context.Error
exception Package_error = Project_context.Package_error
exception Stop_watch = Watcher.Stop
exception Build_failure = Compiler_scheduler.Build_failure
exception Parse_failure = Package_build.Parse_failure
exception Reported_failure of string
exception Full_rebuild_required

type retained_build = {root_config: Config.t; stats: Build_types.t}

(* Build kind controls which persistent markers and diagnostics may be reused.
   Keeping all four states explicit prevents an initial watch build from being
   mistaken for either a disposable command or a retained incremental edit. *)
type compilation_kind =
  | One_shot
  | Initial_watch
  | Incremental_watch
  | Full_watch

type incremental_source = {
  package: Build_types.graph_package;
  module_: Source.module_;
  relative_path: string;
  absolute_path: string;
}

let project_root folder =
  if not (Sys.file_exists folder) then
    raise
      (Error
         ("Could not start Rescript build: Could not write lockfile because \
           the specified project folder does not exist: " ^ folder));
  Unix.realpath folder

let clean ~poll ~seen ~verbosity ~folder ~prod =
  let root = project_root folder in
  let show_progress = verbosity >= 0 in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let colors = Output.colors_enabled ~interactive in
  let print_cleaning ~step target =
    if interactive && show_progress then
      Printf.printf "%s%!"
        (Output.cleaning_command_message ~color:colors ~step target)
  in
  let print_cleaned ~step ~target ~started_at =
    if interactive && show_progress then
      print_endline
        (Output.cleaned_command_message ~color:colors ~step ~target
           ~seconds:(Unix.gettimeofday () -. started_at))
  in
  Build_lock.with_build ~poll (Project_context.workspace_lock_root root)
    (fun ~release:_ ->
      poll ();
      let root_config = Config.load_root root in
      let resolution = Package_resolution.create root_config in
      let visited = Hashtbl.create 32 in
      List.iter
        (fun path -> Hashtbl.replace visited (Unix.realpath path) ())
        seen;
      let cleanup =
        Clean.prepare ~root_config ~resolution ~seen:visited ~root ~prod
          ~is_local:true
      in
      let compiler_assets = "compiler assets" in
      let compiler_started = Unix.gettimeofday () in
      Clean.remove_compiler_assets cleanup ~on_clean:(fun name ->
          if show_progress then
            if interactive then print_cleaning ~step:"1/2" name
            else Printf.printf "Cleaning %s\n%!" name);
      print_cleaned ~step:"1/2" ~target:compiler_assets
        ~started_at:compiler_started;
      poll ();
      let suffixes =
        root_config.package_specs
        |> List.filter_map (fun (spec : Config.package_spec) ->
            if spec.in_source then
              Some (Config.package_spec_suffix root_config spec)
            else None)
        |> String.concat ", "
      in
      let generated_files = suffixes ^ " files" in
      let generated_started = Unix.gettimeofday () in
      print_cleaning ~step:"2/2" generated_files;
      Clean.remove_generated_outputs cleanup;
      poll ();
      print_cleaned ~step:"2/2" ~target:generated_files
        ~started_at:generated_started)

let compiler_args = Compiler_args_command.run

let run_scheduled_modules (stats : Build_types.t) ~compile_step ~namespace_count
    =
  let prepared = Build_types.prepared_exn stats in
  Compiler_scheduler.run ~poll:stats.process_poll
    ~warning_state:stats.retained.warning_state
    ~compile_assets:prepared.compile_assets ~build_state:prepared.build_state
    ~candidates:!(stats.compile_candidates)
    ~mark_compiled:(fun () -> stats.compiled <- stats.compiled + 1)
    ~mark_had_warnings:(fun () -> stats.had_warnings <- true)
    ~progress:stats.progress ~compile_step ~namespace_count
    ~verbosity:stats.verbosity

let run_namespace_jobs (stats : Build_types.t) =
  let jobs = List.rev !(stats.namespace_jobs) in
  let started_at = Unix.gettimeofday () in
  Fun.protect
    ~finally:(fun () ->
      stats.parse_seconds <-
        stats.parse_seconds +. (Unix.gettimeofday () -. started_at))
    (fun () ->
      let results =
        Process.run_parallel ?poll:stats.process_poll (List.map fst jobs)
      in
      List.iter2 (fun (_, finish) result -> finish result) jobs results);
  List.length jobs

let write_source_dirs (root_config : Config.t) (stats : Build_types.t) =
  let packages =
    Hashtbl.to_seq_values stats.retained.graph_packages
    |> List.of_seq
    |> List.sort (fun (left : Build_types.graph_package) right ->
        String.compare left.graph_root right.graph_root)
  in
  packages
  |> List.iter (fun package ->
      if package.Build_types.graph_root <> root_config.root then
        File_util.remove_file
          (File_util.path_of_parts package.graph_root
             ["lib"; "bs"; ".sourcedirs.json"]));
  let local_packages =
    List.filter (fun package -> package.Build_types.graph_is_local) packages
  in
  let source_directories package =
    package.Build_types.graph_modules
    |> List.map (fun module_ -> Filename.dirname module_.Source.implementation)
    |> List.sort_uniq String.compare
  in
  let relative_package_root package =
    if package.Build_types.graph_root = root_config.root then ""
    else Project_context.relative_to root_config.root package.graph_root
  in
  let dirs =
    local_packages
    |> List.concat_map (fun package ->
        let relative_root = relative_package_root package in
        source_directories package
        |> List.map (fun directory ->
            if relative_root = "" then directory
            else Filename.concat relative_root directory))
    |> List.sort_uniq String.compare
  in
  let package_roots = Hashtbl.create 16 in
  local_packages
  |> List.iter (fun package ->
      package.Build_types.graph_dependency_directories
      |> List.iter (fun dependency ->
          Hashtbl.replace package_roots dependency.Build_types.declaration.name
            dependency.directory));
  let package_roots =
    Hashtbl.to_seq package_roots
    |> List.of_seq
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  let scans =
    local_packages
    |> List.map (fun package ->
        let relative_root = relative_package_root package in
        let build_root =
          if relative_root = "" then File_util.path_of_parts "" ["lib"; "bs"]
          else File_util.path_of_parts relative_root ["lib"; "bs"]
        in
        Source_dirs.
          {
            build_root;
            scan_dirs = source_directories package;
            also_scan_build_root = true;
          })
    |> List.sort (fun (left : Source_dirs.scan) right ->
        String.compare left.build_root right.build_root)
  in
  Source_dirs.write ~root:root_config.root ~dirs ~packages:package_roots ~scans

let write_build_ninja (stats : Build_types.t) =
  (* This empty file is a cache-invalidation marker consumed by editor tooling,
     not a serialized build plan. Only commands that reconstruct the project
     graph call this function. *)
  Hashtbl.iter
    (fun _ package ->
      let path =
        Filename.concat package.Build_types.graph_build_dir "build.ninja"
      in
      let channel = open_out_bin path in
      close_out channel)
    stats.retained.graph_packages

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
        Hashtbl.find_opt previous.stats.retained.source_index normalized_path
      with
      | Some (package_root, module_, relative_path, absolute_path) ->
        let package =
          match
            Hashtbl.find_opt previous.stats.retained.graph_packages package_root
          with
          | Some package -> package
          | None -> raise Full_rebuild_required
        in
        sources := {package; module_; relative_path; absolute_path} :: !sources
      | None -> raise Full_rebuild_required)
  in
  List.iter
    (fun (change : Watcher.change) ->
      match change.kind with
      | Watcher.Added | Watcher.Removed -> raise Full_rebuild_required
      | Watcher.Modified ->
        add (Platform.normalize_path_for_comparison change.path))
    changes;
  previous.stats.retained.pending_parse_paths |> Hashtbl.to_seq_keys
  |> List.of_seq |> List.sort String.compare |> List.iter add;
  List.rev !sources

let prepare_incremental previous changes (stats : Build_types.t) =
  (* A retained edit reparses only the reported paths, then replaces the
     affected modules' dependency edges in memory. This keeps the long-lived
     graph coherent without rediscovering the package tree. *)
  let sources = incremental_sources previous changes in
  let prepared =
    match stats.retained.prepared with
    | Some prepared -> prepared
    | None -> raise Full_rebuild_required
  in
  let bsc = prepared.compiler_context.bsc_path in
  let started_at = Unix.gettimeofday () in
  sources
  |> List.map (fun source ->
      Source.compiler_basename source.package.graph_compile_config
        source.module_.Source.name)
  |> List.sort_uniq String.compare
  |> List.iter (fun name ->
      Output.debug ~verbosity:stats.verbosity
        ("Generating AST for module: " ^ name));
  let parse_completed =
    Output.Progress.start_grouped stats.progress ~step:"1/2" ~symbol:"🧱 "
      ~label:"Parsing"
      (List.map
         (fun source ->
           source.package.graph_root ^ "\000" ^ source.module_.Source.name)
         sources)
  in
  let results =
    sources
    |> List.map (fun source ->
        Compiler_process.parse_job ~bsc
          ~build_dir:source.package.graph_build_dir
          ~config:source.package.graph_compile_config source.relative_path)
    |> Process.run_parallel ?poll:stats.process_poll
         ~on_complete:parse_completed
  in
  let affected_modules = Hashtbl.create (List.length sources) in
  let dependencies_changed = ref false in
  List.iter2
    (fun source result ->
      Hashtbl.replace stats.forced_parse_paths source.absolute_path ();
      Hashtbl.replace stats.preparse_results source.absolute_path result;
      if Process.succeeded result && result.stderr <> "" then
        Hashtbl.replace stats.preparse_stderr source.absolute_path result.stderr;
      (try
         let modified = (Unix.stat source.absolute_path).Unix.st_mtime in
         Hashtbl.replace source.package.graph_source_mtimes source.relative_path
           modified
       with Unix.Unix_error _ | Sys_error _ -> raise Full_rebuild_required);
      let key =
        Source.compiler_basename source.package.graph_compile_config
          source.module_.Source.name
      in
      let parse_failed = not (Process.succeeded result) in
      let parse_failed =
        match Hashtbl.find_opt affected_modules key with
        | Some (_, _, previous_failed) -> previous_failed || parse_failed
        | None -> parse_failed
      in
      Hashtbl.replace affected_modules key
        (source.package, source.module_, parse_failed))
    sources results;
  Hashtbl.iter
    (fun key (package, module_, changed_parse_failed) ->
      if not changed_parse_failed then
        let dependencies path =
          Compiler_process.ast_dependencies
            ~build_dir:package.Build_types.graph_build_dir
            (Source.ast_path path)
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
          match Hashtbl.find_opt stats.retained.global_modules key with
          | Some node -> node
          | None -> raise Full_rebuild_required
        in
        if node.raw_dependencies <> raw_dependencies then (
          dependencies_changed := true;
          node.raw_dependencies <- raw_dependencies;
          Build_state.set_dependencies prepared.build_state ~key
            (Build_preparation.resolved_dependencies
               stats.retained.global_modules
               stats.retained.namespace_maps_by_name node)))
    affected_modules;
  stats.parse_seconds <- Unix.gettimeofday () -. started_at;
  if !dependencies_changed || stats.retained.graph_has_cycle then (
    let cycle =
      Build_preparation.find_cycle stats.retained.global_modules
        stats.retained.namespace_maps prepared.build_state
    in
    stats.retained.graph_has_cycle <- Option.is_some cycle;
    cycle)
  else None

let run_with_warning_state ~process_poll ~poll ~warning_state ~previous ~changes
    ~compilation_kind ~no_timing ~seen ~verbosity ~folder ~prod ~features
    ~warn_error ~watch ~after_build ~filter ~on_state =
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
  let process_poll =
    match process_poll with
    | Some _ -> Some poll
    | None ->
      if watch || (interactive && show_progress) then Some poll else None
  in
  let is_rebuild = compilation_kind = Incremental_watch in
  let should_write_build_ninja =
    match compilation_kind with
    | One_shot | Full_watch -> true
    | Initial_watch | Incremental_watch -> false
  in
  let output_kind =
    match compilation_kind with
    | Initial_watch -> Some "initial"
    | Incremental_watch -> Some "incremental"
    | One_shot | Full_watch -> None
  in
  let root = project_root folder in
  let root_config =
    match previous with
    | Some previous -> previous.root_config
    | None -> Config.load_root root
  in
  Output.debug ~verbosity
    (Printf.sprintf "Created project context Single project: %S at %S for %S"
       root_config.name root_config.path root_config.root);
  let visited = Hashtbl.create 32 in
  let stats : Build_types.t =
    match previous with
    | Some previous ->
      Build_types.create_incremental ~previous:previous.stats ~poll
        ~process_poll ~progress ~verbosity
    | None ->
      Build_types.create ~warning_state ~poll ~process_poll ~progress ~verbosity
  in
  let parse_messages () = List.rev !(stats.parse_messages) in
  let parse_output messages =
    messages
    |> List.map (function
        | Build_types.Parse_warning output | Build_types.Parse_error output ->
        output)
    |> String.concat ""
  in
  let parse_failed messages =
    List.exists
      (function
        | Build_types.Parse_error _ -> true
        | Build_types.Parse_warning _ -> false)
      messages
  in
  (* A watch build must retain the attempted state even when later parsing or
     compilation fails, because its successful ASTs and artifact inventory are
     needed to recover incrementally on the next edit. Publish ownership before
     any fallible phase starts. *)
  on_state {root_config; stats};
  List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
  let finalize_logs () =
    Output.Progress.finish progress;
    Hashtbl.iter
      (fun package_root () -> Compiler_log.finalize package_root)
      stats.initialized_logs;
    Hashtbl.clear stats.initialized_logs
  in
  let artifacts_cleaned = ref false in
  let cleanup_after_build () =
    if not !artifacts_cleaned then (
      List.iter (fun cleanup -> cleanup ()) !(stats.compile_cleanup);
      stats.compile_cleanup := [];
      List.iter File_util.remove_file !(stats.deferred_artifact_cleanup);
      stats.deferred_artifact_cleanup := [];
      artifacts_cleaned := true)
  in
  let build_ninja_written = ref false in
  let write_build_ninja_once () =
    if should_write_build_ninja && not !build_ninja_written then (
      write_build_ninja stats;
      build_ninja_written := true)
  in
  let phase_seconds seconds = if no_timing then 0. else seconds in
  let parse_step = if is_rebuild then "1/2" else "2/3" in
  let compile_step = if is_rebuild then "2/2" else "3/3" in
  let prepare_report ~success ~compile_seconds =
    (* Finalize compiler logs before replaying warnings and configuration
       diagnostics so persisted and terminal output describe the same completed
       build, in deterministic module and package order. *)
    finalize_logs ();
    if stats.attempt_kind = Build_types.Full_attempt then
      write_source_dirs root_config stats;
    if show_progress then
      if interactive then
        if success then
          print_endline
            (Output.compiling_message ~color:colors ~step:compile_step
               ~count:stats.compiled ~seconds:compile_seconds)
        else
          prerr_endline
            (Output.compilation_failed_message ~color:colors ~step:compile_step
               ~count:stats.compiled ~seconds:compile_seconds)
      else (
        (match compilation_kind with
        | One_shot | Initial_watch | Full_watch ->
          Printf.printf "Cleaned %d/%d\n%!" stats.cleaned stats.previous_asts
        | Incremental_watch -> ());
        Printf.printf "Parsed %d source files\n%!" stats.parsed;
        if success then Printf.printf "Compiled %d modules\n%!" stats.compiled
        else Printf.eprintf "Compiled %d modules\n%!" stats.compiled);
    let diagnostics =
      match compilation_kind with
      | Incremental_watch | Full_watch -> []
      | One_shot | Initial_watch ->
        stats.diagnostics |> List.rev |> List.sort_uniq String.compare
    in
    let warning_entries = Warning_state.entries stats.retained.warning_state in
    warning_entries
    |> List.iter (fun entry -> prerr_string entry.Warning_state.output);
    if warning_entries <> [] && diagnostics = [] then prerr_newline ();
    flush stderr;
    if diagnostics <> [] then
      diagnostics
      |> List.map (fun diagnostic ->
          if colors then Output.yellow diagnostic else diagnostic)
      |> String.concat "\n\n" |> prerr_endline;
    diagnostics
  in
  let report_completion diagnostics =
    if interactive && show_progress then
      let seconds =
        if no_timing then 0. else Unix.gettimeofday () -. started_at
      in
      Printf.printf "\n%s\n%!"
        (Output.finished_compilation_message ~kind:output_kind
           ~warnings:
             (stats.had_warnings || diagnostics <> []
             || Warning_state.entries stats.retained.warning_state <> [])
           ~seconds)
    else if watch && show_progress then
      Printf.printf "Finished %scompilation\n%!"
        (match compilation_kind with
        | Initial_watch -> "initial "
        | Incremental_watch -> "incremental "
        | One_shot | Full_watch -> "")
  in
  let report ~success ~compile_seconds =
    let diagnostics = prepare_report ~success ~compile_seconds in
    if success then report_completion diagnostics
  in
  let report_failure ~compile_seconds output =
    report ~success:false ~compile_seconds;
    prerr_string output;
    prerr_newline ();
    cleanup_after_build ();
    write_build_ninja_once ();
    raise
      (Reported_failure
         ("Incremental build failed. Error: \027[2K\r  Failed to Compile. "
        ^ "See Errors Above"))
  in
  let report_parse_failure output =
    finalize_logs ();
    (if interactive && show_progress then
       prerr_endline
         (Output.parsing_failed_message ~color:colors
            ~step:(if is_rebuild then "1/2" else "2/3")
            ~seconds:(if no_timing then 0. else stats.parse_seconds))
     else if show_progress then
       match compilation_kind with
       | One_shot | Initial_watch | Full_watch ->
         Printf.printf "Cleaned %d/%d\n%!" stats.cleaned stats.previous_asts
       | Incremental_watch -> ());
    prerr_endline output;
    cleanup_after_build ();
    write_build_ninja_once ();
    raise
      (Reported_failure
         "Incremental build failed. Error: \027[2K\r  Could not parse Source \
          Files")
  in
  let format_cycle cycle
      (by_key : (string, Build_preparation.cycle_node) Hashtbl.t) =
    let format_node name =
      match Hashtbl.find_opt by_key name with
      | None -> name
      | Some node -> (
        match node.source_path with
        | Some source_path ->
          let absolute = Filename.concat node.package_root source_path in
          Printf.sprintf "%s (%s)" node.display_name
            (Project_context.display_path ~root:root_config.root absolute)
        | None ->
          Printf.sprintf "%s (%s namespace map)" node.display_name
            (Project_context.display_path ~root:root_config.root
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
    let cycle =
      match (previous, changes) with
      | Some previous, Some changes ->
        prepare_incremental previous changes stats
      | Some _, None -> raise Full_rebuild_required
      | None, _ ->
        Build_preparation.run ~root_config ~prod ~features ~warn_error ~filter
          ~watch ~stats ~parse_step ~on_cleanup:(fun seconds ->
            if interactive && show_progress && not is_rebuild then (
              if stats.compiler_cleaned then
                print_endline
                  (Output.compiler_cleanup_message ~color:colors ~step:"1/3");
              print_endline
                (Output.cleanup_message ~color:colors ~step:"1/3"
                   ~cleaned:stats.cleaned ~total:stats.previous_asts
                   ~seconds:(phase_seconds seconds))))
    in
    poll ();
    if stats.compiler_cleaned && show_progress && not interactive then
      print_endline "Cleaned previous build due to compiler update";
    Option.iter
      (fun (cycle_info : Build_preparation.cycle_info) ->
        List.iter
          (fun name -> Hashtbl.replace stats.blocked_modules name ())
          cycle_info.blocked)
      cycle;
    let root_package =
      match Hashtbl.find_opt stats.retained.graph_packages root with
      | Some package -> package
      | None -> raise (Error ("Package graph was not prepared for " ^ root))
    in
    Package_build.prepare_tree ~seen:visited ~package:root_package ~watch ~stats;
    (Build_types.prepared_exn stats).freshness_initialized <- true;
    let parse_messages = parse_messages () in
    let parse_output = parse_output parse_messages in
    if parse_failed parse_messages then raise (Parse_failure parse_output);
    poll ();
    let namespace_count =
      try run_namespace_jobs stats
      with Build_failure output ->
        raise (Parse_failure (parse_output ^ output))
    in
    Output.Progress.finish progress;
    if interactive && show_progress then
      print_endline
        (Output.parsing_message ~color:colors ~step:parse_step
           ~count:stats.parsed
           ~seconds:(phase_seconds stats.parse_seconds));
    flush stdout;
    prerr_string parse_output;
    flush stderr;
    let compile_started = Unix.gettimeofday () in
    (try run_scheduled_modules stats ~compile_step ~namespace_count
     with Build_failure output ->
       if Option.is_none stats.failure then stats.failure <- Some output);
    Output.Progress.finish progress;
    let compile_seconds =
      phase_seconds (Unix.gettimeofday () -. compile_started)
    in
    match (stats.failure, cycle) with
    | Some output, _ -> report_failure ~compile_seconds output
    | None, Some cycle_info ->
      let output = format_cycle cycle_info.cycle cycle_info.nodes_by_key in
      cycle_info.cycle
      |> List.filter_map (Hashtbl.find_opt cycle_info.nodes_by_key)
      |> List.map (fun node -> node.Build_preparation.package_root)
      |> List.sort_uniq String.compare
      |> List.iter (fun package_root -> Compiler_log.append package_root output);
      report_failure ~compile_seconds output
    | None, None ->
      let diagnostics = prepare_report ~success:true ~compile_seconds in
      Option.iter
        (fun (prepared : Build_types.prepared) ->
          let context = prepared.compiler_context in
          Hashtbl.iter
            (fun _ package ->
              let package_context =
                {
                  context with
                  build_root = package.Build_types.graph_build_owner;
                  package_output_specs =
                    Compiler_info.package_output_specs
                      package.graph_compile_config;
                }
              in
              Compiler_info.write_package package_context package.graph_config)
            stats.retained.graph_packages)
        stats.retained.prepared;
      if compilation_kind = One_shot then report_completion diagnostics;
      cleanup_after_build ();
      write_build_ninja_once ();
      release_build_lock ();
      Option.iter
        (fun command -> After_build.run ?poll:process_poll ~root command)
        after_build;
      if compilation_kind <> One_shot then report_completion diagnostics
  in
  Build_lock.with_build ~poll (Project_context.workspace_lock_root root)
    (fun ~release:release_build_lock ->
      Fun.protect
        ~finally:(fun () ->
          cleanup_after_build ();
          finalize_logs ())
        (fun () ->
          try execute ~release_build_lock with
          | Build_failure output -> report_failure ~compile_seconds:0. output
          | Parse_failure output -> report_parse_failure output));
  {root_config; stats}

let run ~poll ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch
    ~after_build ~filter ~no_timing =
  try
    run_with_warning_state ~warning_state:(Warning_state.create ())
      ~process_poll:(Some poll) ~poll ~previous:None ~changes:None
      ~compilation_kind:One_shot ~no_timing ~seen ~verbosity ~folder ~prod
      ~features ~warn_error ~watch ~after_build ~filter ~on_state:(fun _ -> ())
    |> ignore
  with Reported_failure message -> raise (Error message)

let watch ~verbosity ~folder ~prod ~features ~warn_error ~after_build ~filter
    ~clear_screen =
  let root = project_root folder in
  let warning_state = Warning_state.create () in
  let initial_build = ref true in
  let retained = ref None in
  let force_full_rebuild = ref false in
  let build ~poll ~changes =
    let is_initial = !initial_build in
    initial_build := false;
    try
      let run ?previous ?changes compilation_kind =
        let attempted = ref None in
        try
          run_with_warning_state ~process_poll:(Some poll) ~poll ~warning_state
            ~previous ~changes ~compilation_kind ~no_timing:false ~seen:[]
            ~verbosity ~folder ~prod ~features ~warn_error ~watch:true
            ~after_build ~filter ~on_state:(fun state ->
              attempted := Some state)
        with exn ->
          (* Failed initial and incremental attempts still own useful parsed
             state. Full reconstruction failures do not, because their graph may
             be only partially discovered. *)
          (match (compilation_kind, !attempted) with
          | (Initial_watch | Incremental_watch), Some state ->
            retained := Some state
          | (One_shot | Full_watch), _ | _, None -> ());
          raise exn
      in
      let next =
        match (!retained, changes, !force_full_rebuild) with
        | Some previous, Some changes, false -> (
          try run ~previous ~changes Incremental_watch
          with Full_rebuild_required ->
            force_full_rebuild := true;
            run Full_watch)
        | Some _, _, true -> run Full_watch
        | None, _, _ -> run (if is_initial then Initial_watch else Full_watch)
        | Some _, None, false -> run Full_watch
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
  Watcher.run ~root ~prod ~features ~filter ~clear_screen
    ~show_progress:(verbosity >= 0) ~verbosity ~build
