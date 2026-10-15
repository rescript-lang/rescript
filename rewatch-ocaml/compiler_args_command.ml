let error message = raise (Project_context.Error message)

let runtime_path resolution package_root =
  try
    Toolchain.runtime
      ~find_package:
        (Package_resolution.dependency_path resolution ~package_root)
  with Toolchain.Error message -> error message

let source_error path message =
  error (Printf.sprintf "Could not read source file %s: %s" path message)

(* An absolute path with "." and ".." resolved textually, without following
   symlinks, like Rust's [to_lexical_absolute]. Rust rebuilds the path from
   its components, so on Windows every separator becomes a backslash, also
   after the drive of a path such as [D:/project]. *)
let lexical_absolute path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let rec normalize path =
    let directory = Filename.dirname path in
    let base = Filename.basename path in
    if directory = path then path
    else if base = Filename.current_dir_name then normalize directory
    else if base = Filename.parent_dir_name then
      Filename.dirname (normalize directory)
    else Filename.concat (normalize directory) base
  in
  let path = normalize path in
  if Sys.win32 then String.map (fun c -> if c = '/' then '\\' else c) path
  else path

let run ~verbosity path =
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
  let package_config =
    match Project_context.nearest_config_path (Filename.dirname source) with
    | Some path -> Config.load path
    | None -> error "could not find a rescript.json parent"
  in
  let root_config = Project_context.workspace_config_for package_config in
  Output.debug ~channel:stderr ~verbosity
    (Project_context.describe package_config ~workspace:root_config);
  let resolution =
    Package_resolution.create
      ~diagnostic_mode:Package_resolution.Suppress_diagnostics root_config
  in
  let config = Config.with_root_options package_config root_config in
  let relative = Project_context.relative_to config.root source in
  let contents =
    try File_util.read_file source with
    | Sys_error message -> source_error path message
    | Unix.Unix_error (unix_error, _, _) ->
      source_error path (Unix.error_message unix_error)
  in
  let parser_args =
    Compiler_args.parser_arguments ~config ~contents ~path:relative
  in
  let source_kind =
    if Filename.check_suffix source ".resi" then Source.Interface
    else Source.Implementation
  in
  let has_interface =
    match source_kind with
    | Source.Implementation -> File_util.exists (source ^ "i")
    | Source.Interface -> false
  in
  let dependencies =
    (if Config.source_is_dev config relative then
       List.map (fun dependency -> (false, dependency)) config.dev_dependencies
     else [])
    @ List.map (fun dependency -> (true, dependency)) config.dependencies
  in
  let dependency_dirs =
    dependencies
    |> List.filter_map (fun (required, (dependency : Config.dependency)) ->
        match
          Package_resolution.dependency_path resolution
            ~package_root:config.root dependency.name
        with
        | Some directory -> Some (Build_artifacts.lib_path directory "ocaml")
        | None when not required -> None
        | None ->
          error
            (Printf.sprintf "Expected to find dependent package %s of %s"
               dependency.name config.name))
  in
  let runtime = runtime_path resolution config.root in
  (* As in Rust rewatch, editors get the project root spelled the way they
     named the source, for example through a symlink or a Windows short path:
     they compile the incremental copy of the source under that root. *)
  let project_root =
    Project_context.nearest_config_path
      (Filename.dirname (lexical_absolute path))
    |> Option.fold ~none:config.root ~some:Filename.dirname
  in
  let compiler_args =
    Compiler_args.compiler_arguments ~project_root ~config ~runtime
      ~dependency_dirs
      ~module_name:(Source.module_name source)
      ~source_kind ~has_interface ~watch:false ~gentype_dependency_args:[]
      ~path:relative
  in
  Yojson.Safe.pretty_to_string
    (`Assoc
       [
         ( "compiler_args",
           `List (List.map (fun value -> `String value) compiler_args) );
         ( "parser_args",
           `List (List.map (fun value -> `String value) parser_args) );
       ])
