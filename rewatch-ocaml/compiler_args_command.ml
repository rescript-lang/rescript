let error message = raise (Project_context.Error message)

let rec nearest_config directory =
  if Config.exists_in_root directory then Config.path_in_root directory
  else
    let parent = Filename.dirname directory in
    if parent = directory then error "could not find a rescript.json parent"
    else nearest_config parent

let runtime_path root =
  try Toolchain.runtime ~find_package:(Project_context.dependency_path root)
  with Toolchain.Error message -> error message

let source_error path message =
  error (Printf.sprintf "Could not read source file %s: %s" path message)

let run path =
  let source =
    try
      Filename.concat
        (Platform.canonicalize_path (Filename.dirname path))
        (Filename.basename path)
    with
    | Sys_error message -> source_error path message
    | Unix.Unix_error (unix_error, _, _) ->
      source_error path (Unix.error_message unix_error)
  in
  if
    not
      (Filename.check_suffix source ".res"
      || Filename.check_suffix source ".resi")
  then error "compiler-args expects a .res or .resi source file";
  let package_config = Config.load (nearest_config (Filename.dirname source)) in
  let root = Project_context.workspace_lock_root package_config.root in
  let root_config_path = Config.path_in_root root in
  let root_config =
    if root <> package_config.root && Config.exists_in_root root then
      Config.load root_config_path
    else package_config
  in
  let config = Build_artifacts.with_root_options package_config root_config in
  let relative = Project_context.relative_to config.root source in
  let contents =
    try File_util.read_file source
    with
    | Sys_error message -> source_error path message
    | Unix.Unix_error (unix_error, _, _) ->
      source_error path (Unix.error_message unix_error)
  in
  let parser_args =
    Compiler_args.compiler_flags
      ~ppx_flags:
        (Compiler_args.filter_ppx_flags config.ppx_flags contents)
      ~source_maps:false ~watch:false ~gentype:false config
    @ [
        "-absname";
        "-bs-ast";
        "-o";
        Source.ast_path relative;
        Filename.concat
          (Filename.concat Filename.parent_dir_name Filename.parent_dir_name)
        relative;
      ]
  in
  let is_interface = Filename.check_suffix source ".resi" in
  let has_interface = not is_interface && Sys.file_exists (source ^ "i") in
  let dependencies =
    (if Config.source_is_dev config relative then
       List.map (fun dependency -> (false, dependency)) config.dev_dependencies
     else [])
    @ List.map (fun dependency -> (true, dependency)) config.dependencies
  in
  let dependency_dirs =
    dependencies
    |> List.filter_map (fun (required, (dependency : Config.dependency)) ->
         match Project_context.dependency_path config.root dependency.name with
         | Some directory -> Some (Build_artifacts.lib_path directory "ocaml")
         | None when not required -> None
         | None ->
           error
             (Printf.sprintf "Expected to find dependent package %s of %s"
                dependency.name config.name))
  in
  let runtime = runtime_path config.root in
  let compiler_args =
    let ast = Source.ast_path relative in
    let namespace_args =
      Compiler_args.namespace_args config (Source.module_name source)
    in
    let interface_args =
      if not is_interface && has_interface then ["-bs-read-cmi"] else []
    in
    let output_args =
      if is_interface then []
      else
        List.concat_map
          (fun spec ->
            [
              "-bs-package-output";
              Compiler_args.package_output config relative spec;
            ])
          config.package_specs
    in
    namespace_args @ interface_args
    @ ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
    @ ["-runtime-path"; runtime]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ Compiler_args.compiler_flags ~source_maps:true ~watch:false ~gentype:true
        config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  Yojson.Safe.pretty_to_string
    (`Assoc
      [
        ( "compiler_args",
          `List (List.map (fun value -> `String value) compiler_args) );
        ( "parser_args",
          `List (List.map (fun value -> `String value) parser_args) );
      ])
