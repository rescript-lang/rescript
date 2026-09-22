exception Error = Project_context.Error

let prepare_removed_modules ~(package : Package_plan.t)
    (attempt : Build_attempt.t) =
  let cleanup =
    match Build_attempt.find_cleanup_result attempt package.root with
    | Some result -> result
    | None ->
      raise (Error ("Package cleanup was not prepared for " ^ package.root))
  in
  let removed = Hashtbl.create (List.length cleanup.removed_modules) in
  List.iter
    (fun module_name ->
      Hashtbl.replace removed module_name ();
      Hashtbl.replace attempt.removed_modules module_name ())
    cleanup.removed_modules;
  removed

let rec prepare_tree ~seen ~(package : Package_plan.t) ~prepared ~watch
    ~(attempt : Build_attempt.t) =
  let root = package.root in
  Hashtbl.replace seen root ();
  attempt.diagnostics <-
    List.rev_append
      (Package_diagnostics.for_package ~is_local:package.is_local package.config)
      attempt.diagnostics;
  List.iter
    (fun (dependency : Package_plan.dependency) ->
      let root = dependency.directory in
      if not (Hashtbl.mem seen root) then
        match Build_session.find_package_plan attempt.session root with
        | None -> ()
        | Some dependency_package ->
          prepare_tree ~seen ~package:dependency_package ~prepared ~watch
            ~attempt)
    package.dependencies;
  File_util.ensure_dir package.build_dir;
  File_util.ensure_dir package.ocaml_dir;
  Compiler_log.initialize root;
  Build_attempt.mark_log_initialized attempt root;
  let prepared_package =
    match Hashtbl.find_opt prepared.Build_session.packages root with
    | Some package -> package
    | None ->
      raise (Error ("Package build was not prepared for " ^ package.root))
  in
  let removed_module_names = prepare_removed_modules ~package attempt in
  let parse_dirty_modules =
    Package_parse.run ~package ~prepared ~prepared_package ~attempt
      ~removed_module_names
  in
  Package_compilation.prepare ~package ~prepared ~prepared_package ~attempt
    ~watch ~removed_module_names ~parse_dirty_modules
