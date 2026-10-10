exception Error = Project_context.Error

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
  let present_public_outputs =
    match Build_session.find_public_outputs attempt.session root with
    | Some outputs -> outputs
    | None -> raise (Error ("Package cleanup was not prepared for " ^ root))
  in
  let removed_module_names = Hashtbl.create 8 in
  Build_attempt.removed_package_modules attempt root
  |> List.iter (fun module_name ->
      Hashtbl.replace removed_module_names module_name ());
  let parse_dirty_modules =
    Package_parse.run ~package ~prepared ~attempt ~removed_module_names
  in
  Package_compilation.prepare ~package ~prepared ~prepared_package ~attempt
    ~watch ~present_public_outputs ~removed_module_names ~parse_dirty_modules
