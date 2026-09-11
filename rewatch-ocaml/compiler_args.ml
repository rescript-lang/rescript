let ppx_is_enabled ~bisect_enabled flag contents =
  if String_util.contains flag "bisect" then bisect_enabled
  else
    not
      ((String_util.contains flag "graphql-ppx"
       || String_util.contains flag "graphql_ppx")
       && not (String_util.contains contents "%graphql")
      || String_util.contains flag "spice"
         && not (String_util.contains contents "@spice")
      || String_util.contains flag "rescript-relay"
         && not (String_util.contains contents "%relay")
      || String_util.contains flag "re-formality"
         && not (String_util.contains contents "%form"))

let filter_ppx_flags ?bisect_enabled flags contents =
  let bisect_enabled =
    Option.value bisect_enabled
      ~default:(Option.is_some (Sys.getenv_opt "BISECT_ENABLE"))
  in
  List.filter
    (function
      | [] -> false
      | flag :: _ -> ppx_is_enabled ~bisect_enabled flag contents)
    flags

let source_map_args (config : Config.t) ~watch =
  if config.source_map_dev && not watch then ["-bs-source-map"; "false"]
  else config.source_map_args

let compiler_flags ?(ppx_flags = []) ~source_maps ~watch ~gentype
    (config : Config.t) =
  let ppx_args =
    ppx_flags
    |> List.concat_map (function
      | [] -> []
      | flag :: arguments ->
        let executable =
          match Project_context.dependency_path config.root flag with
          | Some path -> path
          | None -> flag
        in
        ["-ppx"; String.concat " " (executable :: arguments)])
  in
  let source_map_args =
    if source_maps then source_map_args config ~watch else []
  in
  if source_maps then
    ppx_args @ config.jsx_args @ source_map_args @ config.compiler_flags
    @ config.warning_flags
    @ (if gentype then config.gentype_args else [])
    @ config.experimental_args
  else
    ppx_args @ config.jsx_args @ config.experimental_args @ config.warning_flags
    @ config.compiler_flags

let with_local_warning_policy ~is_local (config : Config.t) =
  if is_local then config else {config with warning_flags = []}

let package_output (config : Config.t) path (spec : Config.package_spec) =
  let output_dir = Build_artifacts.relative_output_directory path spec in
  Printf.sprintf "%s:%s:%s"
    (Config.module_format_name spec.module_format)
    output_dir
    (Config.package_spec_suffix config spec)

let gentype_dependency_args_from_paths (config : Config.t) dependencies =
  if config.gentype_args = [] then []
  else
    let paths = Hashtbl.create (List.length dependencies) in
    List.iter
      (fun ((dependency : Config.dependency), path) ->
        Hashtbl.replace paths dependency.name path)
      dependencies;
    config.dependencies
    |> List.concat_map (fun (dependency : Config.dependency) ->
        match Hashtbl.find_opt paths dependency.name with
        | None -> []
        | Some path -> ["-bs-gentype-dep-path"; dependency.name ^ "=" ^ path])

let namespace_args (config : Config.t) module_name =
  match config.namespace with
  | Config.No_namespace -> []
  | Config.Namespace namespace -> ["-bs-ns"; namespace]
  | Config.Namespace_with_entry {name; entry} ->
    if entry = module_name then ["-open"; "@" ^ name]
    else ["-bs-ns"; "@" ^ name]

let parser_arguments ~(config : Config.t) ~contents ~path =
  compiler_flags
    ~ppx_flags:(filter_ppx_flags config.ppx_flags contents)
    ~source_maps:false ~watch:false ~gentype:false config
  @ [
      "-absname";
      "-bs-ast";
      "-o";
      Source.ast_path path;
      Filename.concat
        (Filename.concat Filename.parent_dir_name Filename.parent_dir_name)
        path;
    ]

let compiler_common_arguments ~(config : Config.t) ~runtime ~dependency_dirs
    ~watch ~gentype_dependency_args =
  ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
  @ ["-runtime-path"; runtime]
  @ List.concat_map (fun directory -> ["-I"; directory]) dependency_dirs
  @ compiler_flags ~source_maps:true ~watch ~gentype:true config
  @ gentype_dependency_args
  @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]

let compiler_arguments_with_common ~(config : Config.t) ~common_args
    ~module_name ~is_interface ~has_interface ~path =
  let interface_args =
    if (not is_interface) && has_interface then ["-bs-read-cmi"] else []
  in
  let output_args =
    if is_interface then []
    else
      List.concat_map
        (fun spec -> ["-bs-package-output"; package_output config path spec])
        config.package_specs
  in
  namespace_args config module_name
  @ interface_args @ common_args @ output_args
  @ [Source.ast_path path]

let compiler_arguments ~(config : Config.t) ~runtime ~dependency_dirs
    ~module_name ~is_interface ~has_interface ~watch ~gentype_dependency_args
    ~path =
  compiler_arguments_with_common ~config
    ~common_args:
      (compiler_common_arguments ~config ~runtime ~dependency_dirs ~watch
         ~gentype_dependency_args)
    ~module_name ~is_interface ~has_interface ~path
