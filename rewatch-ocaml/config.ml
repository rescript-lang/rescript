type module_format = Esmodule | Commonjs

type package_spec = {
  module_format: module_format;
  in_source: bool;
  suffix: string option;
}

type source = {
  dir: string;
  recurse: bool;
  is_dev: bool;
  feature: string option;
}

type dependency = {name: string; features: string list option}

type t = {
  path: string;
  root: string;
  name: string;
  sources: source list;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  compiler_flags: string list;
  package_specs: package_spec list;
  suffix: string;
  namespace: string option;
  namespace_entry: string option;
  features: (string * string list) list;
  warning_flags: string list;
  ppx_flags: string list list;
  jsx_args: string list;
  source_map_args: string list;
  source_map_dev: bool;
  experimental_args: string list;
  gentype_args: string list;
  js_post_build: string option;
  allowed_dependents: string list option;
  deprecation_diagnostics: string list;
  diagnostics: string list;
}

exception Error of string

let fail path message = raise (Error (Printf.sprintf "%s: %s" path message))
let member name fields = List.assoc_opt name fields

let optional_member name fields =
  match member name fields with None | Some `Null -> None | value -> value

let rec deduplicate_last = function
  | [] -> []
  | ((name, _) as field) :: rest ->
    if List.mem_assoc name rest then deduplicate_last rest
    else field :: deduplicate_last rest

let last_member name fields = List.assoc_opt name (deduplicate_last fields)

let last_optional_member name fields =
  match last_member name fields with None | Some `Null -> None | value -> value

let reject_duplicate_fields path context known fields =
  let seen = Hashtbl.create (List.length fields) in
  List.iter
    (fun (name, _) ->
      if List.mem name known then
        if Hashtbl.mem seen name then
          fail path (Printf.sprintf "duplicate field %S in %s" name context)
        else Hashtbl.add seen name ())
    fields

let path_in_root root =
  let current = Filename.concat root "rescript.json" in
  if Sys.file_exists current then current else Filename.concat root "bsconfig.json"

let exists_in_root root =
  Sys.file_exists (Filename.concat root "rescript.json")
  || Sys.file_exists (Filename.concat root "bsconfig.json")

let namespace_from_package_name name =
  let buffer = Buffer.create (String.length name) in
  let capitalize = ref true in
  String.iter
    (fun character ->
      match character with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
        Buffer.add_char buffer
          (if !capitalize then Char.uppercase_ascii character else character);
        capitalize := false
      | '/' | '-' -> capitalize := true
      | _ -> ())
    name;
  Buffer.contents buffer

let string path field = function
  | `String value -> value
  | _ -> fail path (Printf.sprintf "field %S must be a string" field)

let bool path field = function
  | `Bool value -> value
  | _ -> fail path (Printf.sprintf "field %S must be a boolean" field)

let strings path field = function
  | `List values -> List.map (string path field) values
  | _ -> fail path (Printf.sprintf "field %S must be an array of strings" field)

let compiler_flags path field = function
  | `List values ->
    values |> List.concat_map (function
      | `String value -> String.split_on_char ' ' value |> List.filter ((<>) "")
      | `List values ->
        values |> List.concat_map (fun value ->
          string path field value |> String.split_on_char ' ' |> List.filter ((<>) ""))
      | _ -> fail path (Printf.sprintf "field %S entries must be strings or arrays" field))
  | _ -> fail path (Printf.sprintf "field %S must be an array" field)

let dependency_name path = function
  | `String value -> {name = value; features = None}
  | `Assoc fields -> (
    reject_duplicate_fields path "dependency" ["name"; "features"] fields;
    match member "name" fields with
    | Some value ->
      let features = match optional_member "features" fields with
        | None -> None
        | Some value -> Some (strings path "features" value)
      in
      {name = string path "name" value; features}
    | None -> fail path "dependency object is missing field \"name\"")
  | _ -> fail path "dependency must be a string or object"

let parse_dependencies path field fields =
  match optional_member field fields with
  | None -> []
  | Some (`List values) -> List.map (dependency_name path) values
  | Some _ -> fail path (Printf.sprintf "field %S must be an array" field)

let dependency_alias path modern legacy fields =
  match member modern fields, member legacy fields with
  | Some _, Some _ ->
    fail path (Printf.sprintf "fields %S and %S cannot both be set" modern legacy)
  | Some _, None -> parse_dependencies path modern fields
  | None, Some _ -> parse_dependencies path legacy fields
  | None, None -> []

let rec sources_of_json path inherited_dir forced_dev inherited_feature = function
  | `String dir ->
    [
      {
        dir = Filename.concat inherited_dir dir;
        recurse = false;
        is_dev = Option.value forced_dev ~default:false;
        feature = inherited_feature;
      };
    ]
  | `Assoc fields ->
    reject_duplicate_fields path "source" ["dir"; "subdirs"; "type"; "feature"]
      fields;
    let dir =
      match member "dir" fields with
      | Some value -> Filename.concat inherited_dir (string path "dir" value)
      | None -> fail path "source object is missing field \"dir\""
    in
    let declared_dev =
    match optional_member "type" fields with
      | None -> false
      | Some (`String "dev") -> true
      | Some (`String _) -> false
      | Some _ -> fail path "source field \"type\" must be a string"
    in
    let is_dev = Option.value forced_dev ~default:declared_dev
    in
    let feature =
      match optional_member "feature" fields with
      | None -> inherited_feature
      | Some value -> Some (string path "feature" value)
    in
    let recurse, children =
      match optional_member "subdirs" fields with
      | None -> (false, [])
      | Some (`Bool value) -> (value, [])
      | Some (`List values) ->
        ( false,
          List.concat_map
            (sources_of_json path dir (Some is_dev) feature)
            values )
      | Some _ ->
        fail path "source field \"subdirs\" must be a boolean or array"
    in
    {dir; recurse; is_dev; feature} :: children
  | _ -> fail path "source must be a string or object"

let parse_sources path fields =
  match optional_member "sources" fields with
  | None -> []
  | Some (`List values) ->
    List.concat_map (sources_of_json path "" None None) values
  | Some value -> sources_of_json path "" None None value

let supported_fields =
  [
    "name";
    "sources";
    "dependencies";
    "bs-dependencies";
    "dev-dependencies";
    "bs-dev-dependencies";
    "compiler-flags";
    "bsc-flags";
    "package-specs";
    "suffix";
    "namespace";
    "namespace-entry";
    "allowed-dependents";
    "features";
    "ignored-dirs";
    "generators";
    "cut-generators";
    "pp-flags";
    "entries";
    "bs-external-includes";
    "warnings";
    "ppx-flags";
    "jsx";
    "gentypeconfig";
    "reanalyze";
    "editor";
    "experimental-features";
    "js-post-build";
    "sourceMap";
  ]

let nested_unknown_fields parent supported = function
  | `Assoc fields ->
    fields
    |> List.filter_map (fun (name, _) ->
         if List.mem name supported then None
         else Some (Printf.sprintf "%s.?.%s" parent name))
  | _ -> []

let unknown_fields fields =
  fields
  |> List.concat_map (fun (name, value) ->
       match name with
       | "warnings" -> nested_unknown_fields name ["number"; "error"] value
       | "jsx" ->
         nested_unknown_fields name
           ["version"; "module"; "mode"; "v3-dependencies"; "preserve"]
           value
       | "gentypeconfig" ->
         nested_unknown_fields name
           [
             "module";
             "moduleResolution";
             "exportInterfaces";
             "generatedFileExtension";
             "shims";
             "debug";
           ]
           value
       | "js-post-build" -> nested_unknown_fields name ["cmd"] value
       | _ -> if List.mem name supported_fields then [] else [name])

let parse_package_spec path = function
  | `Assoc fields ->
    reject_duplicate_fields path "package-specs entry"
      ["module"; "in-source"; "suffix"] fields;
    let module_format =
      match member "module" fields with
      | Some (`String ("esmodule" | "es6")) -> Esmodule
      | Some (`String ("commonjs" | "cjs")) -> Commonjs
      | None -> fail path "package-specs entry is missing field \"module\""
      | Some value ->
        fail path
          (Printf.sprintf "unsupported package module %S"
             (Yojson.Safe.to_string value))
    in
    let in_source =
      match member "in-source" fields with
      | None -> true
      | Some value -> bool path "in-source" value
    in
    let suffix =
      match optional_member "suffix" fields with
      | None -> None
      | Some value -> Some (string path "suffix" value)
    in
    {module_format; in_source; suffix}
  | _ -> fail path "package-specs entries must be objects"

let package_specs_use_alias alias = function
  | `Assoc fields -> member "module" fields = Some (`String alias)
  | `List values ->
    List.exists
      (function
        | `Assoc fields -> member "module" fields = Some (`String alias)
        | _ -> false)
      values
  | _ -> false

let gentype_args path configured_suffix package_specs_value sources dependencies = function
  | `Assoc fields ->
    reject_duplicate_fields path "gentypeconfig"
      [
        "module";
        "moduleResolution";
        "exportInterfaces";
        "generatedFileExtension";
        "shims";
        "debug";
      ]
      fields;
    let module_ =
      match optional_member "module" fields with
      | None -> (
        match package_specs_value with
        | Some (`Assoc package_spec) -> (
          match member "module" package_spec with
          | Some (`String ("esmodule" | "es6")) ->
            ["-bs-gentype-module"; "esmodule"]
          | Some (`String ("commonjs" | "cjs")) ->
            ["-bs-gentype-module"; "commonjs"]
          | _ -> [])
        | _ -> [])
      | Some (`String ("esmodule" | "commonjs" as value)) -> ["-bs-gentype-module"; value]
      | Some _ -> fail path "field \"gentypeconfig.module\" must be \"esmodule\" or \"commonjs\""
    in
    let module_resolution =
      match optional_member "moduleResolution" fields with
      | None -> []
      | Some (`String ("node" | "node16" | "bundler" as value)) ->
        ["-bs-gentype-module-resolution"; value]
      | Some _ -> fail path "field \"gentypeconfig.moduleResolution\" is invalid"
    in
    let export_interfaces =
      match optional_member "exportInterfaces" fields with
      | None | Some (`Bool false) -> []
      | Some (`Bool true) -> ["-bs-gentype-export-interfaces"]
      | Some _ -> fail path "field \"gentypeconfig.exportInterfaces\" must be a boolean"
    in
    let generated_extension =
      match optional_member "generatedFileExtension" fields with
      | None -> []
      | Some value -> ["-bs-gentype-generated-extension"; string path "gentypeconfig.generatedFileExtension" value]
    in
    let shims =
      let pairs =
        match member "shims" fields with
        | None -> []
        | Some (`Assoc values) ->
          List.map
            (fun (from_, target) ->
              (from_, string path "gentypeconfig.shims" target))
            values
        | Some (`List values) ->
          List.map
            (fun value ->
              let value = string path "gentypeconfig.shims" value in
              match String.index_opt value '=' with
              | Some separator ->
                let from_ = String.sub value 0 separator |> String.trim in
                let target =
                  String.sub value (separator + 1)
                    (String.length value - separator - 1)
                  |> String.trim
                in
                (from_, target)
              | None -> fail path "gentypeconfig.shims entries must contain =")
            values
        | Some _ ->
          fail path "field \"gentypeconfig.shims\" must be an object or array"
      in
      let by_source = Hashtbl.create (List.length pairs) in
      List.iter (fun (from_, target) -> Hashtbl.replace by_source from_ target) pairs;
      Hashtbl.to_seq by_source |> List.of_seq |> List.sort compare
      |> List.concat_map (fun (from_, target) ->
           ["-bs-gentype-shim"; from_ ^ "=" ^ target])
    in
    let debug =
      match member "debug" fields with
      | None -> []
      | Some (`Assoc values) ->
        values |> deduplicate_last |> List.sort compare
        |> List.concat_map (fun (name, value) ->
          match value with
          | `Bool true -> ["-bs-gentype-debug"; name]
          | `Bool false -> []
          | _ -> fail path "gentypeconfig.debug values must be booleans")
      | Some _ -> fail path "field \"gentypeconfig.debug\" must be an object"
    in
    let suffix_args =
      match configured_suffix with
      | None -> []
      | Some suffix -> ["-bs-gentype-suffix"; suffix]
    in
    ["-bs-gentype"] @ module_ @ module_resolution @ export_interfaces
    @ generated_extension @ suffix_args @ shims @ debug
    @ List.concat_map (fun (dependency : dependency) -> ["-bs-gentype-dep"; dependency.name]) dependencies
    @ List.concat_map (fun (source : source) -> ["-bs-gentype-source-dir"; source.dir]) sources
  | _ -> fail path "field \"gentypeconfig\" must be an object"

let load path =
  let path = Unix.realpath path in
  let root = Filename.dirname path in
  let json =
    try Yojson.Safe.from_file path
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
    ]
    fields;
  let name =
    match member "name" fields with
    | Some value -> string path "name" value
    | None -> fail path "missing required field \"name\""
  in
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
    match optional_member "namespace-entry" fields, namespace with
    | None, _ -> None
    | Some _, None -> fail path "field \"namespace-entry\" requires a namespace"
    | Some value, Some _ -> Some (string path "namespace-entry" value)
  in
  let compiler_flags =
    match member "compiler-flags" fields, member "bsc-flags" fields with
    | Some _, Some _ -> fail path "fields \"compiler-flags\" and \"bsc-flags\" cannot both be set"
    | Some `Null, None | None, Some `Null -> []
    | Some value, None -> compiler_flags path "compiler-flags" value
    | None, Some value -> compiler_flags path "bsc-flags" value
    | None, None -> []
  in
  let warning_flags =
    match optional_member "warnings" fields with
    | None -> []
    | Some (`Assoc warning_fields) ->
      reject_duplicate_fields path "warnings" ["number"; "error"]
        warning_fields;
      let number = match optional_member "number" warning_fields with
        | None -> [] | Some value -> ["-w"; string path "number" value] in
      let error = match optional_member "error" warning_fields with
        | Some (`Bool true) -> ["-warn-error"; "A"]
        | Some (`String value) -> ["-warn-error"; value]
        | None | Some (`Bool false) -> []
        | Some _ -> fail path "field \"warnings.error\" must be a boolean or string"
      in number @ error
    | Some _ -> fail path "field \"warnings\" must be an object"
  in
  let ppx_flags =
    match optional_member "ppx-flags" fields with
    | None -> []
    | Some (`List values) ->
      List.map (function
        | `String value -> [value]
        | `List values -> List.map (string path "ppx-flags") values
        | _ -> fail path "field \"ppx-flags\" entries must be strings or arrays") values
    | Some _ -> fail path "field \"ppx-flags\" must be an array"
  in
  let jsx_args =
    match optional_member "jsx" fields with
    | None -> []
    | Some (`Assoc jsx) ->
      reject_duplicate_fields path "jsx"
        ["version"; "module"; "mode"; "v3-dependencies"; "preserve"]
        jsx;
      let version = match optional_member "version" jsx with
        | None -> []
        | Some (`Int 4) -> ["-bs-jsx"; "4"]
        | Some _ -> fail path "field \"jsx.version\" must be 4"
      in
      let module_ = match optional_member "module" jsx with
        | None -> [] | Some value -> ["-bs-jsx-module"; string path "jsx.module" value] in
      let mode = match optional_member "mode" jsx with
        | None -> []
        | Some (`String ("classic" | "automatic" as value)) -> ["-bs-jsx-mode"; value]
        | Some _ -> fail path "field \"jsx.mode\" must be \"classic\" or \"automatic\""
      in
      let preserve = match optional_member "preserve" jsx with
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
        "sourceMap true is unsupported; use an object with enabled and mode fields or false"
    | Some (`Assoc options) ->
      let mode =
        match last_member "mode" options with
        | Some (`String ("linked" | "inline" | "hidden" as value)) ->
          value
        | None -> fail path "sourceMap is missing field \"mode\""
        | Some _ ->
          fail path "sourceMap.mode must be one of linked, inline, hidden"
      in
      let dev_only =
        match last_member "enabled" options with
        | Some (`String "always") -> false
        | Some (`String "dev") -> true
        | None -> fail path "sourceMap is missing field \"enabled\""
        | Some _ ->
          fail path "sourceMap.enabled must be \"always\" or \"dev\""
      in
      let content = match last_optional_member "sourcesContent" options with
        | None -> [] | Some (`Bool value) -> ["-bs-source-map-sources-content"; string_of_bool value]
        | Some _ -> fail path "field \"sourceMap.sourcesContent\" must be a boolean" in
      let root = match last_optional_member "sourceRoot" options with
        | None -> [] | Some value -> ["-bs-source-map-root"; string path "sourceMap.sourceRoot" value] in
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
                  "Unknown experimental feature '%s'. Available features: LetUnwrap"
                  name);
           match value with
           | `Bool true -> ["-enable-experimental"; name]
           | `Bool false -> []
           | _ ->
             fail path
               "experimental-features: invalid type: feature values must be booleans")
    | Some _ ->
      fail path
        "Could not read rescript.json: experimental-features: invalid type: expected an object"
  in
  let sources = parse_sources path fields in
  let dependencies = dependency_alias path "dependencies" "bs-dependencies" fields in
  let dev_dependencies = dependency_alias path "dev-dependencies" "bs-dev-dependencies" fields in
  let gentype_args =
    match optional_member "gentypeconfig" fields with
    | None -> []
    | Some value ->
      gentype_args path configured_suffix (member "package-specs" fields)
        sources dependencies value
  in
  let js_post_build =
    match optional_member "js-post-build" fields with
    | None -> None
    | Some (`Assoc fields) ->
      reject_duplicate_fields path "js-post-build" ["cmd"] fields;
      (match member "cmd" fields with
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
      values |> deduplicate_last |> List.map
        (fun (name, value) -> (name, strings path "features" value))
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
    @ (match member "package-specs" fields with
      | Some value ->
        [
          ( "cjs",
            "  - module 'cjs' in package-specs — use 'commonjs' instead" );
          ( "es6",
            "  - module 'es6' in package-specs — use 'esmodule' instead" );
        ]
        |> List.filter_map (fun (alias, message) ->
             if package_specs_use_alias alias value then Some message else None)
      | None -> [])
  in
  let deprecation_diagnostics =
    if deprecated = [] then []
     else
       [
         Printf.sprintf
           "\n\nPackage '%s' uses deprecated config (support will be removed in a future version):\n%s"
           name
           (String.concat "\n" deprecated);
       ]
  in
  let diagnostics =
    deprecation_diagnostics
    @ (unsupported_fields
      |> List.map (fun field ->
           Printf.sprintf
             "The field '%s' found in the package config of '%s' is not supported by ReScript 12's new build system."
             field name))
    @ (unknown_fields fields
      |> List.map (fun field ->
           Printf.sprintf
             "Unknown field '%s' found in the package config of '%s'. This option will be ignored."
             field name))
  in
  {
    path;
    root;
    name;
    sources;
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
