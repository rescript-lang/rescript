let contains_text value text =
  try
    ignore (Str.search_forward (Str.regexp_string text) value 0);
    true
  with Not_found -> false

let ppx_is_enabled ~bisect_enabled flag contents =
  if contains_text flag "bisect" then bisect_enabled
  else
    not
      ((contains_text flag "graphql-ppx" || contains_text flag "graphql_ppx")
      && not (contains_text contents "%graphql")
      || (contains_text flag "spice" && not (contains_text contents "@spice"))
      || (contains_text flag "rescript-relay"
         && not (contains_text contents "%relay"))
      || (contains_text flag "re-formality"
         && not (contains_text contents "%form")))

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

let compiler_flags ?(ppx_flags = []) ~source_maps ~watch ~gentype
    (config : Config.t) =
  let ppx_args =
    ppx_flags |> List.concat_map (function
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
    if not source_maps then []
    else if config.source_map_dev && not watch then
      ["-bs-source-map"; "false"]
    else config.source_map_args
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
  let directory = Filename.dirname path in
  let output_dir =
    if spec.in_source then directory
    else
      Filename.concat
        (match spec.module_format with
        | Config.Esmodule -> Build_artifacts.lib_path "" "es6"
        | Config.Commonjs -> Build_artifacts.lib_path "" "js")
        directory
  in
  Printf.sprintf "%s:%s:%s"
    (Config.module_format_name spec.module_format)
    output_dir
    (Config.package_spec_suffix config spec)

let gentype_dependency_args (config : Config.t) =
  if config.gentype_args = [] then []
  else
    config.dependencies |> List.concat_map (fun (dependency : Config.dependency) ->
      match Project_context.dependency_path config.root dependency.name with
      | None -> []
      | Some path -> ["-bs-gentype-dep-path"; dependency.name ^ "=" ^ path])

let namespace_args (config : Config.t) module_name =
  match config.namespace, config.namespace_entry with
  | None, _ -> []
  | Some namespace, Some entry when entry = module_name -> ["-open"; "@" ^ namespace]
  | Some namespace, Some _ -> ["-bs-ns"; "@" ^ namespace]
  | Some namespace, _ -> ["-bs-ns"; namespace]

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

let compiler_arguments ~(config : Config.t) ~runtime ~dependency_dirs
    ~module_name ~is_interface ~has_interface ~watch ~gentype_dependency_args
    ~path =
  let interface_args =
    if not is_interface && has_interface then ["-bs-read-cmi"] else []
  in
  let output_args =
    if is_interface then []
    else
      List.concat_map
        (fun spec -> ["-bs-package-output"; package_output config path spec])
        config.package_specs
  in
  namespace_args config module_name @ interface_args
  @ ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
  @ ["-runtime-path"; runtime]
  @ List.concat_map (fun directory -> ["-I"; directory]) dependency_dirs
  @ compiler_flags ~source_maps:true ~watch ~gentype:true config
  @ gentype_dependency_args
  @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
  @ output_args @ [Source.ast_path path]
