exception Error = Project_context.Error
exception Build_failure = Compiler_scheduler.Build_failure

let prepare_removed_modules ~(package : Build_types.graph_package)
    (stats : Build_types.t) =
  let cleanup =
    match
      Hashtbl.find_opt stats.retained.cleanup_results package.graph_root
    with
    | Some result -> result
    | None ->
      raise
        (Error ("Package cleanup was not prepared for " ^ package.graph_root))
  in
  let removed = Hashtbl.create (List.length cleanup.removed_modules) in
  List.iter
    (fun module_name ->
      Hashtbl.replace removed module_name ();
      Hashtbl.replace stats.removed_modules module_name ())
    cleanup.removed_modules;
  removed

let rec prepare_tree ~seen ~(package : Build_types.graph_package) ~watch
    ~(stats : Build_types.t) =
  let root = package.graph_root in
  Hashtbl.replace seen root ();
  stats.diagnostics <-
    List.rev_append
      (Package_diagnostics.for_package ~is_local:package.graph_is_local
         package.graph_config)
      stats.diagnostics;
  List.iter
    (fun (dependency : Build_types.graph_dependency) ->
      let root = dependency.directory in
      if not (Hashtbl.mem seen root) then
        match Hashtbl.find_opt stats.retained.graph_packages root with
        | None -> ()
        | Some dependency_package -> (
          try prepare_tree ~seen ~package:dependency_package ~watch ~stats
          with Build_failure output ->
            if Option.is_none stats.failure then stats.failure <- Some output))
    package.graph_dependency_directories;
  File_util.ensure_dir package.graph_build_dir;
  File_util.ensure_dir package.graph_ocaml_dir;
  Compiler_log.initialize root;
  Hashtbl.replace stats.initialized_logs root ();
  let removed_module_names = prepare_removed_modules ~package stats in
  let parse_dirty_modules =
    Package_parse.run ~package ~stats ~removed_module_names
  in
  Package_compilation.run ~package ~stats ~watch ~removed_module_names
    ~parse_dirty_modules
