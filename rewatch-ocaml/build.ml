exception Error = Project_context.Error
exception Package_error = Project_context.Package_error
exception Stop_watch = Watcher.Stop
exception Build_failure = Compiler_scheduler.Build_failure
exception Parse_failure = Package_build.Parse_failure
exception Reported_failure of string

open Build_types

let project_root folder =
  if not (Sys.file_exists folder) then
    raise
      (Error
         ("Could not start Rescript build: Could not write lockfile because the specified project folder does not exist: "
         ^ folder));
  Unix.realpath folder

let clean ~seen ~verbosity ~folder ~prod =
  let root = project_root folder in
  let show_plain_progress =
    verbosity >= 0
    && not (Unix.isatty Unix.stdout && Unix.isatty Unix.stderr)
  in
  let on_clean name =
    if show_plain_progress then Printf.printf "Cleaning %s\n%!" name
  in
  Build_lock.with_build (Project_context.workspace_lock_root root)
    (fun ~release:_ ->
      let root_config = Config.load_root root in
      let dependency_context = Project_context.dependency_context root_config in
      let visited = Hashtbl.create 32 in
      List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
      Clean.run ~root_config ~dependency_context ~seen:visited ~root ~prod
        ~is_local:true ~on_clean)

let compiler_args = Compiler_args_command.run

let run_scheduled_modules stats =
  let build_state =
    match stats.build_state with
    | Some state -> state
    | None -> raise (Error "build state was not initialized")
  in
  let compile_assets =
    match stats.compile_assets with
    | Some state -> state
    | None -> raise (Error "compile asset state was not initialized")
  in
  Compiler_scheduler.run ~poll:stats.poll ~warning_state:stats.warning_state
    ~blocked_modules:stats.blocked_modules ~compile_assets ~build_state
    ~scheduled_modules:!(stats.scheduled_modules)
    ~compile_cleanup:!(stats.compile_cleanup)
    ~mark_compiled:(fun () -> stats.compiled <- stats.compiled + 1)
    ~mark_had_warnings:(fun () -> stats.had_warnings <- true)

let run_namespace_jobs stats =
  let jobs = List.rev !(stats.namespace_jobs) in
  let results = Process.run_parallel ~poll:stats.poll (List.map fst jobs) in
  List.iter2 (fun (_, finish) result -> finish result) jobs results

let write_source_dirs (root_config : Config.t) stats =
  let packages =
    Hashtbl.to_seq_values stats.graph_packages |> List.of_seq
    |> List.sort (fun left right -> String.compare left.graph_root right.graph_root)
  in
  packages
  |> List.iter (fun package ->
       if package.graph_root <> root_config.root then
         File_util.remove_file
           (File_util.path_of_parts package.graph_root ["lib"; "bs"; ".sourcedirs.json"]));
  let local_packages = List.filter (fun package -> package.graph_is_local) packages in
  let source_directories package =
    package.graph_modules
    |> List.map (fun module_ -> Filename.dirname module_.Source.implementation)
    |> List.sort_uniq String.compare
  in
  let relative_package_root package =
    if package.graph_root = root_config.root then ""
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
       package.graph_dependency_directories
       |> List.iter (fun ((dependency : Config.dependency), path) ->
            Hashtbl.replace package_roots dependency.name path));
  let package_roots =
    Hashtbl.to_seq package_roots |> List.of_seq
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

let write_build_ninja stats =
  Hashtbl.iter
    (fun _ package ->
      let path = Filename.concat package.graph_build_dir "build.ninja" in
      let channel = open_out_bin path in
      close_out channel)
    stats.graph_packages

let run_with_warning_state ~poll ~warning_state ~compilation_kind ~no_timing
    ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch ~after_build
    ~filter =
  let started_at = Unix.gettimeofday () in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let show_progress = verbosity >= 0 in
  let is_rebuild = compilation_kind = Some "incremental" in
  let should_write_build_ninja = (not watch) || is_rebuild in
  let root = project_root folder in
  let root_config = Config.load_root root in
  if verbosity > 0 then
    Printf.printf "Created project context for %S\n%!" root_config.root;
  let visited = Hashtbl.create 32 in
  let stats = Build_types.create ~warning_state ~poll in
  List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
  let finalize_logs () =
    Hashtbl.iter (fun package_root () -> Compiler_log.finalize package_root)
      stats.initialized_logs;
    Hashtbl.clear stats.initialized_logs
  in
  let outputs_finished = ref false in
  let build_ninja_written = ref false in
  let write_build_ninja_once () =
    if should_write_build_ninja && not !build_ninja_written then (
      write_build_ninja stats;
      build_ninja_written := true)
  in
  let expose_watch_outputs () =
    !(stats.watch_outputs)
    |> List.rev
    |> List.iter (fun (output, pending, _) ->
         if Sys.file_exists pending then (
           File_util.remove_file output;
           Unix.rename pending output))
  in
  let finish_watch_outputs ~success =
    !(stats.watch_outputs)
    |> List.rev
    |> List.iter (fun (output, pending, dirty_ast) ->
         if success then (
           if Sys.file_exists pending then (
             File_util.remove_file output;
             Unix.rename pending output))
         else (
           File_util.remove_file output;
           File_util.remove_file pending;
           File_util.remove_file dirty_ast));
    stats.watch_outputs := [];
    Hashtbl.clear stats.watch_output_paths;
    outputs_finished := true
  in
  let report ~success () =
    finish_watch_outputs ~success;
    finalize_logs ();
    if show_progress && not interactive then
      if watch then (
        if success then Printf.printf "Finished compilation\n%!")
      else (
        Printf.printf "Cleaned %d/%d\nParsed %d source files\n%!" stats.cleaned
          stats.previous_asts stats.parsed;
        if success then Printf.printf "Compiled %d modules\n%!" stats.compiled
        else Printf.eprintf "Compiled %d modules\n%!" stats.compiled);
    let diagnostics =
      if compilation_kind = Some "incremental" then []
      else stats.diagnostics |> List.rev |> List.sort_uniq String.compare
    in
    let warning_entries = Warning_state.entries stats.warning_state in
    warning_entries
    |> List.iter (fun entry -> prerr_string entry.Warning_state.output);
    if warning_entries <> [] && diagnostics = [] then prerr_newline ();
    flush stderr;
    if diagnostics <> [] then (
      let output = String.concat "\n\n" diagnostics in
      prerr_endline (if interactive then Output.yellow output else output));
    if success && interactive && show_progress then
      let seconds =
        if no_timing then 0. else Unix.gettimeofday () -. started_at
      in
      Printf.printf "\n%s\n%!"
        (Output.finished_compilation_message ~kind:compilation_kind
           ~warnings:
             (stats.had_warnings || diagnostics <> []
             || Warning_state.entries stats.warning_state <> [])
           ~seconds)
  in
  let report_failure output =
    write_build_ninja_once ();
    report ~success:false ();
    prerr_string output;
    prerr_newline ();
    raise
      (Reported_failure
         ("Incremental build failed. Error: \027[2K\r  Failed to Compile. "
         ^ "See Errors Above"))
  in
  let report_parse_failure output =
    write_build_ninja_once ();
    finish_watch_outputs ~success:false;
    finalize_logs ();
    if interactive && show_progress then
      prerr_endline
        (Output.parsing_failed_message ~step:(if is_rebuild then "1/2" else "2/3")
           ~seconds:(if no_timing then 0. else stats.parse_seconds))
    else if show_progress then
      Printf.printf "Cleaned %d/%d\n%!" stats.cleaned stats.previous_asts;
    prerr_endline output;
    raise
      (Reported_failure
         "Incremental build failed. Error: \027[2K\r  Could not parse Source Files")
  in
  let format_cycle cycle by_key =
    let format_node name =
      match Hashtbl.find_opt by_key name with
      | None -> name
      | Some node ->
        let absolute = Filename.concat node.package_root node.source_path in
        let module_name = Source.module_name node.source_path in
        let display_name =
          match node.namespace, node.namespace_entry with
          | Some namespace, Some entry when entry <> module_name ->
            namespace ^ "." ^ module_name
          | Some namespace, None -> namespace ^ "." ^ module_name
          | _ -> module_name
        in
        Printf.sprintf "%s (%s)" display_name
          (Project_context.relative_to root_config.root absolute)
    in
    "\nCan't continue... Found a circular dependency in your code:\n"
    ^ (cycle |> List.map format_node |> String.concat "\n → ")
    ^ "\nPossible solutions:\n- Extract shared code into a new module both depend on.\n"
  in
  let phase_seconds seconds = if no_timing then 0. else seconds in
  let parse_step = if is_rebuild then "1/2" else "2/3" in
  let compile_step = if is_rebuild then "2/2" else "3/3" in
  let execute ~release_build_lock =
    poll ();
    let cycle =
      Build_preparation.run ~root_config ~prod ~features ~warn_error ~filter
        ~watch ~stats
        ~on_cleanup:(fun seconds ->
          if interactive && show_progress && not is_rebuild then (
            if stats.compiler_cleaned then
              print_endline (Output.compiler_cleanup_message ~step:"1/3");
            print_endline
              (Output.cleanup_message ~step:"1/3" ~cleaned:stats.cleaned
                 ~total:stats.previous_asts ~seconds:(phase_seconds seconds))))
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
    Package_build.prepare_tree ~root_config
      ~dependency_context:(Project_context.dependency_context root_config)
      ~seen:visited ~folder:root ~prod ~features ~warn_error ~watch ~filter
      ~is_local:true ~stats;
    poll ();
    if interactive && show_progress then
      print_endline
        (Output.parsing_message ~step:parse_step ~count:stats.parsed
           ~seconds:(phase_seconds stats.parse_seconds));
    let compile_started = Unix.gettimeofday () in
    (try
       run_namespace_jobs stats;
       run_scheduled_modules stats
     with Build_failure output ->
       if Option.is_none stats.failure then stats.failure <- Some output);
    if interactive && show_progress then (
      let seconds = phase_seconds (Unix.gettimeofday () -. compile_started) in
      match stats.failure with
      | None ->
        print_endline
          (Output.compiling_message ~step:compile_step ~count:stats.compiled
             ~seconds)
      | Some _ ->
        prerr_endline
          (Output.compilation_failed_message ~step:compile_step
             ~count:stats.compiled ~seconds));
    (match stats.failure, cycle with
    | Some output, _ -> report_failure output
    | None, Some cycle_info ->
      let output =
        format_cycle cycle_info.cycle cycle_info.modules_by_key
      in
      cycle_info.cycle
      |> List.filter_map (Hashtbl.find_opt cycle_info.modules_by_key)
      |> List.map (fun node -> node.package_root)
      |> List.sort_uniq String.compare
      |> List.iter (fun package_root -> Compiler_log.append package_root output);
      report_failure output
    | None, None ->
      Option.iter
        (fun (context : Compiler_info.context) ->
          Hashtbl.iter
            (fun _ package ->
              let package_context =
                {
                  context with
                  build_root = package.graph_build_owner;
                  package_output_specs =
                    Compiler_info.package_output_specs
                      package.graph_compile_config;
                }
              in
              Compiler_info.write_package package_context package.graph_config)
            stats.graph_packages)
        stats.compiler_context;
      write_source_dirs root_config stats;
      write_build_ninja_once ();
      Option.iter
        (fun command ->
          expose_watch_outputs ();
          finish_watch_outputs ~success:true;
          finalize_logs ();
          release_build_lock ();
          After_build.run ~root command)
        after_build;
      report ~success:true ())
  in
  Build_lock.with_build (Project_context.workspace_lock_root root)
    (fun ~release:release_build_lock ->
      Fun.protect
        ~finally:(fun () ->
          List.iter File_util.remove_file !(stats.deferred_artifact_cleanup);
          if not !outputs_finished then finish_watch_outputs ~success:false;
          finalize_logs ())
        (fun () ->
          try execute ~release_build_lock with
          | Build_failure output -> report_failure output
          | Parse_failure output -> report_parse_failure output))

let run ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch ~after_build
    ~filter ~no_timing =
  try
    run_with_warning_state ~warning_state:(Warning_state.create ())
      ~poll:(fun () -> ()) ~compilation_kind:None ~no_timing ~seen ~verbosity
      ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter
  with Reported_failure message -> raise (Error message)

let watch ~verbosity ~folder ~prod ~features ~warn_error ~after_build ~filter
    ~clear_screen =
  let root = project_root folder in
  ignore (Config.load_root root);
  let warning_state = Warning_state.create () in
  let initial_build = ref true in
  let build ~poll =
    let compilation_kind =
      if !initial_build then Some "initial" else Some "incremental"
    in
    initial_build := false;
    try
      run_with_warning_state ~poll ~warning_state ~compilation_kind
        ~no_timing:false ~seen:[] ~verbosity ~folder ~prod ~features ~warn_error
        ~watch:true ~after_build ~filter
    with
    | Reported_failure _ -> ()
    | Package_error message | Error message | Config.Error message
    | Source.Error message
    | Process.Error message -> prerr_endline message
    | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
      prerr_endline (Printexc.to_string exn)
  in
  Watcher.run ~root ~prod ~features ~filter ~clear_screen
    ~show_progress:(verbosity >= 0) ~build
