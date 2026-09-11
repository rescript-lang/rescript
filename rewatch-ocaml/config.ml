include Config_types

open Config_decode

let namespace_from_package_name = Config_decode.namespace_from_package_name

let path_in_root root =
  let current = Filename.concat root "rescript.json" in
  if Sys.file_exists current then current
  else Filename.concat root "bsconfig.json"

let exists_in_root root =
  Sys.file_exists (Filename.concat root "rescript.json")
  || Sys.file_exists (Filename.concat root "bsconfig.json")

let source_is_dev (config : t) relative_path =
  let canonical path =
    try Some (Unix.realpath path) with Unix.Unix_error _ | Sys_error _ -> None
  in
  let source_parent =
    Filename.concat config.root relative_path |> Filename.dirname |> canonical
  in
  match source_parent with
  | None -> false
  | Some source_parent ->
    let comparable = Platform.normalize_path_for_comparison in
    List.exists
      (fun (source : source) ->
        if not source.is_dev then false
        else
          match canonical (Filename.concat config.root source.dir) with
          | None -> false
          | Some directory ->
            comparable source_parent = comparable directory
            || source.recurse
               && String.starts_with
                    ~prefix:(Filename.concat directory "" |> comparable)
                    (comparable source_parent))
      config.sources

let load path =
  let requested_path = path in
  let root =
    try Unix.realpath (Filename.dirname path) with
    | Sys_error message ->
      fail_read requested_path (strip_read_path requested_path message)
    | Unix.Unix_error (error, _, _) ->
      fail_read requested_path (Unix.error_message error)
  in
  let path = Filename.concat root (Filename.basename path) in
  (try
     match (Unix.stat path).Unix.st_kind with
     | Unix.S_DIR -> fail_read path (Unix.error_message Unix.EISDIR)
     | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
     | Unix.S_SOCK ->
       ()
   with
  | Sys_error message -> fail_read path (strip_read_path path message)
  | Unix.Unix_error (error, _, _) -> fail_read path (Unix.error_message error));
  let contents =
    try File_util.read_file path with
    | Sys_error message -> fail_read path (strip_read_path path message)
    | Unix.Unix_error (error, _, _) -> fail_read path (Unix.error_message error)
  in
  let json =
    try Yojson.Safe.from_string contents
    with Yojson.Json_error message -> fail path ("invalid JSON: " ^ message)
  in
  let fields =
    match json with
    | `Assoc fields -> fields
    | _ -> fail path "configuration must be an object"
  in
  reject_duplicate_fields path "configuration"
    [
      "name";
      "sources";
      "package-specs";
      "warnings";
      "suffix";
      "dependencies";
      "bs-dependencies";
      "dev-dependencies";
      "bs-dev-dependencies";
      "features";
      "ppx-flags";
      "compiler-flags";
      "bsc-flags";
      "namespace";
      "jsx";
      "sourceMap";
      "experimental-features";
      "gentypeconfig";
      "js-post-build";
      "editor";
      "reanalyze";
      "namespace-entry";
      "allowed-dependents";
      "path";
    ]
    fields;
  let name =
    match member "name" fields with
    | Some value -> string path "name" value
    | None -> fail path "missing required field \"name\""
  in
  (match member "path" fields with
  | None | Some (`String _) -> ()
  | Some _ -> fail path "field \"path\" must be a string");
  let configured_suffix =
    match optional_member "suffix" fields with
    | None -> None
    | Some value -> Some (string path "suffix" value)
  in
  let suffix = Option.value configured_suffix ~default:".js" in
  let package_specs =
    match optional_member "package-specs" fields with
    | None ->
      [{module_format = Esmodule; in_source = true; suffix = Some ".js"}]
    | Some (`List values) -> List.map (parse_package_spec path) values
    | Some value -> [parse_package_spec path value]
  in
  let seen_package_outputs = Hashtbl.create (List.length package_specs) in
  List.iter
    (fun (spec : package_spec) ->
      let effective_suffix = Option.value spec.suffix ~default:suffix in
      let key = (effective_suffix, spec.in_source) in
      if Hashtbl.mem seen_package_outputs key then
        fail path
          (Printf.sprintf "Duplicate package-spec suffix %S is not allowed."
             effective_suffix);
      Hashtbl.add seen_package_outputs key ())
    package_specs;
  let namespace =
    match optional_member "namespace" fields with
    | None | Some (`Bool false) -> None
    | Some (`Bool true) -> Some (namespace_from_package_name name)
    | Some (`String "true") -> Some (namespace_from_package_name name)
    | Some (`String value) -> Some (namespace_from_package_name value)
    | Some _ -> fail path "field \"namespace\" must be a boolean or string"
  in
  let namespace_entry =
    match (optional_member "namespace-entry" fields, namespace) with
    | None, _ -> None
    | Some _, None -> fail path "field \"namespace-entry\" requires a namespace"
    | Some value, Some _ -> Some (string path "namespace-entry" value)
  in
  let compiler_flags =
    match (member "compiler-flags" fields, member "bsc-flags" fields) with
    | Some _, Some _ ->
      fail path "fields \"compiler-flags\" and \"bsc-flags\" cannot both be set"
    | Some `Null, None | None, Some `Null -> []
    | Some value, None -> compiler_flags path "compiler-flags" value
    | None, Some value -> compiler_flags path "bsc-flags" value
    | None, None -> []
  in
  let warning_flags =
    match optional_member "warnings" fields with
    | None -> []
    | Some (`Assoc warning_fields) ->
      reject_duplicate_fields path "warnings" ["number"; "error"] warning_fields;
      let number =
        match optional_member "number" warning_fields with
        | None -> []
        | Some value -> ["-w"; string path "number" value]
      in
      let error =
        match optional_member "error" warning_fields with
        | Some (`Bool true) -> ["-warn-error"; "A"]
        | Some (`String value) -> ["-warn-error"; value]
        | None | Some (`Bool false) -> []
        | Some _ ->
          fail path "field \"warnings.error\" must be a boolean or string"
      in
      number @ error
    | Some _ -> fail path "field \"warnings\" must be an object"
  in
  let ppx_flags =
    match optional_member "ppx-flags" fields with
    | None -> []
    | Some (`List values) ->
      List.map
        (function
          | `String value -> [value]
          | `List values -> List.map (string path "ppx-flags") values
          | _ ->
            fail path "field \"ppx-flags\" entries must be strings or arrays")
        values
    | Some _ -> fail path "field \"ppx-flags\" must be an array"
  in
  let jsx_args =
    match optional_member "jsx" fields with
    | None -> []
    | Some (`Assoc jsx) ->
      reject_duplicate_fields path "jsx"
        ["version"; "module"; "mode"; "v3-dependencies"; "preserve"]
        jsx;
      let version =
        match optional_member "version" jsx with
        | None -> []
        | Some (`Int 4) -> ["-bs-jsx"; "4"]
        | Some _ -> fail path "field \"jsx.version\" must be 4"
      in
      let module_ =
        match optional_member "module" jsx with
        | None -> []
        | Some value -> ["-bs-jsx-module"; string path "jsx.module" value]
      in
      let mode =
        match optional_member "mode" jsx with
        | None -> []
        | Some (`String (("classic" | "automatic") as value)) ->
          ["-bs-jsx-mode"; value]
        | Some _ ->
          fail path "field \"jsx.mode\" must be \"classic\" or \"automatic\""
      in
      let preserve =
        match optional_member "preserve" jsx with
        | None | Some (`Bool false) -> []
        | Some (`Bool true) -> ["-bs-jsx-preserve"]
        | Some _ -> fail path "field \"jsx.preserve\" must be a boolean"
      in
      (match optional_member "v3-dependencies" jsx with
      | None -> ()
      | Some value -> ignore (strings path "jsx.v3-dependencies" value));
      version @ module_ @ mode @ preserve
    | Some _ -> fail path "field \"jsx\" must be an object"
  in
  let source_map_args, source_map_dev =
    match optional_member "sourceMap" fields with
    | None -> ([], false)
    | Some (`Bool false) -> (["-bs-source-map"; "false"], false)
    | Some (`Bool true) ->
      fail path
        "sourceMap true is unsupported; use an object with enabled and mode \
         fields or false"
    | Some (`Assoc options) ->
      let mode =
        match last_member "mode" options with
        | Some (`String (("linked" | "inline" | "hidden") as value)) -> value
        | None -> fail path "sourceMap is missing field \"mode\""
        | Some _ ->
          fail path "sourceMap.mode must be one of linked, inline, hidden"
      in
      let dev_only =
        match last_member "enabled" options with
        | Some (`String "always") -> false
        | Some (`String "dev") -> true
        | None -> fail path "sourceMap is missing field \"enabled\""
        | Some _ -> fail path "sourceMap.enabled must be \"always\" or \"dev\""
      in
      let content =
        match last_optional_member "sourcesContent" options with
        | None -> []
        | Some (`Bool value) ->
          ["-bs-source-map-sources-content"; string_of_bool value]
        | Some _ ->
          fail path "field \"sourceMap.sourcesContent\" must be a boolean"
      in
      let root =
        match last_optional_member "sourceRoot" options with
        | None -> []
        | Some value ->
          ["-bs-source-map-root"; string path "sourceMap.sourceRoot" value]
      in
      (["-bs-source-map"; mode] @ content @ root, dev_only)
    | Some _ -> fail path "field \"sourceMap\" must be false or an object"
  in
  let experimental_args =
    match optional_member "experimental-features" fields with
    | None -> []
    | Some (`Assoc features) ->
      features |> deduplicate_last
      |> List.concat_map (fun (name, value) ->
          if name <> "LetUnwrap" then
            fail path
              (Printf.sprintf
                 "Unknown experimental feature '%s'. Available features: \
                  LetUnwrap"
                 name);
          match value with
          | `Bool true -> ["-enable-experimental"; name]
          | `Bool false -> []
          | _ ->
            fail path
              "experimental-features: invalid type: feature values must be \
               booleans")
    | Some _ ->
      fail path
        "Could not read rescript.json: experimental-features: invalid type: \
         expected an object"
  in
  let sources_defined = Option.is_some (optional_member "sources" fields) in
  let sources = parse_sources path fields in
  let dependencies =
    dependency_alias path "dependencies" "bs-dependencies" fields
  in
  let dev_dependencies =
    dependency_alias path "dev-dependencies" "bs-dev-dependencies" fields
  in
  let gentype_args =
    match optional_member "gentypeconfig" fields with
    | None -> []
    | Some value ->
      gentype_args path configured_suffix
        (member "package-specs" fields)
        dependencies value
  in
  let js_post_build =
    match optional_member "js-post-build" fields with
    | None -> None
    | Some (`Assoc fields) -> (
      reject_duplicate_fields path "js-post-build" ["cmd"] fields;
      match member "cmd" fields with
      | Some value -> Some (string path "js-post-build.cmd" value)
      | None -> fail path "field \"js-post-build\" is missing \"cmd\"")
    | Some _ -> fail path "field \"js-post-build\" must be an object"
  in
  let allowed_dependents =
    match optional_member "allowed-dependents" fields with
    | None -> None
    | Some value -> Some (strings path "allowed-dependents" value)
  in
  let features =
    match optional_member "features" fields with
    | None -> []
    | Some (`Assoc values) ->
      values |> deduplicate_last
      |> List.map (fun (name, value) -> (name, strings path "features" value))
    | Some _ -> fail path "field \"features\" must be an object"
  in
  let unsupported_fields =
    [
      "ignored-dirs";
      "generators";
      "cut-generators";
      "pp-flags";
      "entries";
      "bs-external-includes";
    ]
    |> List.filter (fun field -> Option.is_some (member field fields))
  in
  let deprecated =
    (if Filename.basename path = "bsconfig.json" then
       ["  - filename 'bsconfig.json' — rename to 'rescript.json'"]
     else [])
    @ ([
         ("bs-dependencies", "dependencies");
         ("bs-dev-dependencies", "dev-dependencies");
         ("bsc-flags", "compiler-flags");
       ]
      |> List.filter_map (fun (field, replacement) ->
          if Option.is_some (member field fields) then
            Some
              (Printf.sprintf "  - field '%s' — use '%s' instead" field
                 replacement)
          else None))
    @
    match member "package-specs" fields with
    | Some value ->
      [
        ("cjs", "  - module 'cjs' in package-specs — use 'commonjs' instead");
        ("es6", "  - module 'es6' in package-specs — use 'esmodule' instead");
      ]
      |> List.filter_map (fun (alias, message) ->
          if package_specs_use_alias alias value then Some message else None)
    | None -> []
  in
  let deprecation_diagnostics =
    if deprecated = [] then []
    else
      [
        Printf.sprintf
          "\n\
           Package '%s' uses deprecated config (support will be removed in a \
           future version):\n\
           %s"
          name
          (String.concat "\n" deprecated);
      ]
  in
  let diagnostics =
    deprecation_diagnostics
    @ (unsupported_fields
      |> List.map (fun field ->
          Printf.sprintf
            "The field '%s' found in the package config of '%s' is not \
             supported by ReScript 12's new build system."
            field name))
    @ (unknown_fields fields
      |> List.map (fun field ->
          Printf.sprintf
            "Unknown field '%s' found in the package config of '%s'. This \
             option will be ignored."
            field name))
  in
  {
    path;
    root;
    file_hash = Digest.string contents |> Digest.to_hex;
    name;
    sources;
    sources_defined;
    dependencies;
    dev_dependencies;
    compiler_flags;
    package_specs;
    suffix;
    namespace;
    namespace_entry;
    features;
    warning_flags;
    ppx_flags;
    jsx_args;
    source_map_args;
    source_map_dev;
    experimental_args;
    gentype_args;
    js_post_build;
    allowed_dependents;
    deprecation_diagnostics;
    diagnostics;
  }

let load_root root = load (path_in_root root)

let package_spec_suffix (config : t) (spec : package_spec) =
  Option.value spec.suffix ~default:config.suffix
let module_format_name = function
  | Esmodule -> "esmodule"
  | Commonjs -> "commonjs"
