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
  features: (string * string list) list;
  warning_flags: string list;
  ignored_dirs: string list;
  ppx_flags: string list;
  jsx_args: string list;
  source_map_args: string list;
  source_map_dev: bool;
  experimental_args: string list;
  js_post_build: string option;
}

exception Error of string

let fail path message = raise (Error (Printf.sprintf "%s: %s" path message))
let member name fields = List.assoc_opt name fields

let string path field = function
  | `String value -> value
  | _ -> fail path (Printf.sprintf "field %S must be a string" field)

let bool path field = function
  | `Bool value -> value
  | _ -> fail path (Printf.sprintf "field %S must be a boolean" field)

let strings path field = function
  | `List values -> List.map (string path field) values
  | _ -> fail path (Printf.sprintf "field %S must be an array of strings" field)

let dependency_name path = function
  | `String value -> {name = value; features = None}
  | `Assoc fields -> (
    match member "name" fields with
    | Some value ->
      let features = match member "features" fields with
        | None -> None
        | Some value -> Some (strings path "features" value)
      in
      {name = string path "name" value; features}
    | None -> fail path "dependency object is missing field \"name\"")
  | _ -> fail path "dependency must be a string or object"

let dependencies path field fields =
  match member field fields with
  | None -> []
  | Some (`List values) -> List.map (dependency_name path) values
  | Some _ -> fail path (Printf.sprintf "field %S must be an array" field)

let rec sources_of_json path inherited_dir inherited_dev inherited_feature = function
  | `String dir ->
    [
      {
        dir = Filename.concat inherited_dir dir;
        recurse = false;
        is_dev = inherited_dev;
        feature = inherited_feature;
      };
    ]
  | `Assoc fields ->
    let dir =
      match member "dir" fields with
      | Some value -> Filename.concat inherited_dir (string path "dir" value)
      | None -> fail path "source object is missing field \"dir\""
    in
    let is_dev =
      match member "type" fields with
      | None -> inherited_dev
      | Some (`String "dev") -> true
      | Some _ -> fail path "source field \"type\" must be \"dev\""
    in
    let feature =
      match member "feature" fields with
      | None -> inherited_feature
      | Some value -> Some (string path "feature" value)
    in
    let recurse, children =
      match member "subdirs" fields with
      | None -> (false, [])
      | Some (`Bool value) -> (value, [])
      | Some (`List values) ->
        (false, List.concat_map (sources_of_json path dir is_dev feature) values)
      | Some _ ->
        fail path "source field \"subdirs\" must be a boolean or array"
    in
    {dir; recurse; is_dev; feature} :: children
  | _ -> fail path "source must be a string or object"

let parse_sources path fields =
  match member "sources" fields with
  | None -> []
  | Some (`List values) ->
    List.concat_map (sources_of_json path "" false None) values
  | Some value -> sources_of_json path "" false None value

let validate_supported_fields path fields =
  let supported =
    [
      "name";
      "sources";
      "dependencies";
      "dev-dependencies";
      "compiler-flags";
      "package-specs";
      "suffix";
      "namespace";
      "features";
      "ignored-dirs";
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
  in
  match
    List.find_opt (fun (name, _) -> not (List.mem name supported)) fields
  with
  | None -> ()
  | Some (name, _) ->
    fail path
      (Printf.sprintf
         "configuration field %S is not supported by the experimental OCaml \
          port yet"
         name)

let parse_package_spec path default_suffix = function
  | `String module_name ->
    let module_format =
      match module_name with
      | "esmodule" -> Esmodule
      | "commonjs" -> Commonjs
      | _ ->
        fail path (Printf.sprintf "unsupported package module %S" module_name)
    in
    {module_format; in_source = true; suffix = Some default_suffix}
  | `Assoc fields ->
    let module_format =
      match member "module" fields with
      | None | Some (`String "esmodule") -> Esmodule
      | Some (`String "commonjs") -> Commonjs
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
      match member "suffix" fields with
      | None -> None
      | Some value -> Some (string path "suffix" value)
    in
    {module_format; in_source; suffix}
  | _ -> fail path "package-specs entries must be strings or objects"

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
  validate_supported_fields path fields;
  let name =
    match member "name" fields with
    | Some value -> string path "name" value
    | None -> fail path "missing required field \"name\""
  in
  let suffix =
    match member "suffix" fields with
    | None -> ".js"
    | Some value -> string path "suffix" value
  in
  let package_specs =
    match member "package-specs" fields with
    | None -> [{module_format = Esmodule; in_source = true; suffix = None}]
    | Some (`List values) -> List.map (parse_package_spec path suffix) values
    | Some value -> [parse_package_spec path suffix value]
  in
  let namespace =
    match member "namespace" fields with
    | None | Some (`Bool false) -> None
    | Some (`Bool true) -> Some name
    | Some (`String value) -> Some value
    | Some _ -> fail path "field \"namespace\" must be a boolean or string"
  in
  let compiler_flags =
    match member "compiler-flags" fields with
    | None -> []
    | Some value -> strings path "compiler-flags" value
  in
  let warning_flags =
    match member "warnings" fields with
    | None -> []
    | Some (`Assoc warning_fields) ->
      let number = match member "number" warning_fields with
        | None -> [] | Some value -> ["-w"; string path "number" value] in
      let error = match member "error" warning_fields with
        | Some (`Bool true) -> ["-warn-error"; "A"]
        | Some (`String value) -> ["-warn-error"; value]
        | None | Some (`Bool false) -> []
        | Some _ -> fail path "field \"warnings.error\" must be a boolean or string"
      in number @ error
    | Some _ -> fail path "field \"warnings\" must be an object"
  in
  let ppx_flags =
    match member "ppx-flags" fields with
    | None -> []
    | Some value -> strings path "ppx-flags" value
  in
  let jsx_args =
    match member "jsx" fields with
    | None -> []
    | Some (`Assoc jsx) ->
      let version = match member "version" jsx with
        | None -> []
        | Some (`Int 4) -> ["-bs-jsx"; "4"]
        | Some _ -> fail path "field \"jsx.version\" must be 4"
      in
      let module_ = match member "module" jsx with
        | None -> [] | Some value -> ["-bs-jsx-module"; string path "jsx.module" value] in
      let mode = match member "mode" jsx with
        | None -> []
        | Some (`String ("classic" | "automatic" as value)) -> ["-bs-jsx-mode"; value]
        | Some _ -> fail path "field \"jsx.mode\" must be \"classic\" or \"automatic\""
      in
      let preserve = match member "preserve" jsx with
        | None | Some (`Bool false) -> []
        | Some (`Bool true) -> ["-bs-jsx-preserve"]
        | Some _ -> fail path "field \"jsx.preserve\" must be a boolean"
      in version @ module_ @ mode @ preserve
    | Some _ -> fail path "field \"jsx\" must be an object"
  in
  let source_map_args, source_map_dev =
    match member "sourceMap" fields with
    | None -> ([], false)
    | Some (`Bool false) -> (["-bs-source-map"; "false"], false)
    | Some (`Assoc options) ->
      let mode = match member "mode" options with
        | None -> "linked"
        | Some (`String ("linked" | "inline" | "hidden" as value)) -> value
        | Some _ -> fail path "field \"sourceMap.mode\" is invalid"
      in
      let enabled, dev_only = match member "enabled" options with
        | None | Some (`Bool true) -> (true, false)
        | Some (`Bool false) -> (false, false)
        | Some (`String "dev") -> (true, true)
        | Some _ -> fail path "field \"sourceMap.enabled\" is invalid"
      in
      if not enabled then (["-bs-source-map"; "false"], false) else
      let content = match member "sourcesContent" options with
        | None -> [] | Some (`Bool value) -> ["-bs-source-map-sources-content"; string_of_bool value]
        | Some _ -> fail path "field \"sourceMap.sourcesContent\" must be a boolean" in
      let root = match member "sourceRoot" options with
        | None -> [] | Some value -> ["-bs-source-map-root"; string path "sourceMap.sourceRoot" value] in
      (["-bs-source-map"; mode] @ content @ root, dev_only)
    | Some _ -> fail path "field \"sourceMap\" must be false or an object"
  in
  let experimental_args =
    match member "experimental-features" fields with
    | None -> []
    | Some (`Assoc features) -> features |> List.concat_map (fun (name, value) ->
      match value with
      | `Bool true when name = "LetUnwrap" -> ["-enable-experimental"; name]
      | `Bool false when name = "LetUnwrap" -> []
      | `Bool _ -> fail path ("unsupported experimental feature \"" ^ name ^ "\"")
      | _ -> fail path "experimental feature values must be booleans")
    | Some _ -> fail path "field \"experimental-features\" must be an object"
  in
  let js_post_build =
    match member "js-post-build" fields with
    | None -> None
    | Some (`Assoc fields) ->
      (match member "cmd" fields with
      | Some value -> Some (string path "js-post-build.cmd" value)
      | None -> fail path "field \"js-post-build\" is missing \"cmd\"")
    | Some _ -> fail path "field \"js-post-build\" must be an object"
  in
  let features =
    match member "features" fields with
    | None -> []
    | Some (`Assoc values) ->
      List.map
        (fun (name, value) -> (name, strings path "features" value))
        values
    | Some _ -> fail path "field \"features\" must be an object"
  in
  let ignored_dirs =
    match member "ignored-dirs" fields with
    | None -> []
    | Some value -> strings path "ignored-dirs" value
  in
  {
    path;
    root;
    name;
    sources = parse_sources path fields;
    dependencies = dependencies path "dependencies" fields;
    dev_dependencies = dependencies path "dev-dependencies" fields;
    compiler_flags = warning_flags @ compiler_flags;
    package_specs;
    suffix;
    namespace;
    features;
    warning_flags;
    ignored_dirs;
    ppx_flags;
    jsx_args;
    source_map_args;
    source_map_dev;
    experimental_args;
    js_post_build;
  }

let package_spec_suffix (config : t) (spec : package_spec) =
  Option.value spec.suffix ~default:config.suffix
let module_format_name = function
  | Esmodule -> "esmodule"
  | Commonjs -> "commonjs"
