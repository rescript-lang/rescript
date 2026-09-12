exception Error = Project_context.Error

let prepare_removed_modules ~(package : Build_types.graph_package)
    (attempt : Build_attempt.t) =
  let cleanup =
    match Build_attempt.find_cleanup_result attempt package.graph_root with
    | Some result -> result
    | None ->
      raise
        (Error ("Package cleanup was not prepared for " ^ package.graph_root))
  in
  let removed = Hashtbl.create (List.length cleanup.removed_modules) in
  List.iter
    (fun module_name ->
      Hashtbl.replace removed module_name ();
      Hashtbl.replace attempt.removed_modules module_name ())
    cleanup.removed_modules;
  removed

let rec prepare_tree ~seen ~(package : Build_types.graph_package) ~prepared
    ~watch ~(attempt : Build_attempt.t) =
  let root = package.graph_root in
  Hashtbl.replace seen root ();
  attempt.diagnostics <-
    List.rev_append
      (Package_diagnostics.for_package ~is_local:package.graph_is_local
         package.graph_config)
      attempt.diagnostics;
  List.iter
    (fun (dependency : Build_types.graph_dependency) ->
      let root = dependency.directory in
      if not (Hashtbl.mem seen root) then
        match Build_session.find_graph_package attempt.session root with
        | None -> ()
        | Some dependency_package ->
          prepare_tree ~seen ~package:dependency_package ~prepared ~watch
            ~attempt)
    package.graph_dependency_directories;
  File_util.ensure_dir package.graph_build_dir;
  File_util.ensure_dir package.graph_ocaml_dir;
  Compiler_log.initialize root;
  Build_attempt.mark_log_initialized attempt root;
  let prepared_package =
    match Hashtbl.find_opt prepared.Build_types.packages root with
    | Some package -> package
    | None ->
      raise (Error ("Package build was not prepared for " ^ package.graph_root))
  in
  let removed_module_names = prepare_removed_modules ~package attempt in
  let parse_dirty_modules =
    Package_parse.run ~package ~prepared ~prepared_package ~attempt
      ~removed_module_names
  in
  Package_compilation.run ~package ~prepared ~prepared_package ~attempt ~watch
    ~removed_module_names ~parse_dirty_modules
