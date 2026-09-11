open Config_types

exception Error = Config_types.Error

let fail path message = raise (Error (Printf.sprintf "%s: %s" path message))

let fail_read path message =
  raise (Error (Printf.sprintf "Could not read '%s': %s" path message))

let strip_read_path path message =
  let prefix = path ^ ": " in
  if String.starts_with ~prefix message then
    String.sub message (String.length prefix)
      (String.length message - String.length prefix)
  else message

let member name fields = List.assoc_opt name fields

let optional_member name fields =
  match member name fields with
  | None | Some `Null -> None
  | value -> value

let rec deduplicate_last = function
  | [] -> []
  | ((name, _) as field) :: rest ->
    if List.mem_assoc name rest then deduplicate_last rest
    else field :: deduplicate_last rest

let last_member name fields = List.assoc_opt name (deduplicate_last fields)

let last_optional_member name fields =
  match last_member name fields with
  | None | Some `Null -> None
  | value -> value

let reject_duplicate_fields path context known fields =
  let seen = Hashtbl.create (List.length fields) in
  List.iter
    (fun (name, _) ->
      if List.mem name known then
        if Hashtbl.mem seen name then
          fail path (Printf.sprintf "duplicate field %S in %s" name context)
        else Hashtbl.add seen name ())
    fields

let string path field = function
  | `String value -> value
  | _ -> fail path (Printf.sprintf "field %S must be a string" field)

let bool path field = function
  | `Bool value -> value
  | _ -> fail path (Printf.sprintf "field %S must be a boolean" field)

let strings path field = function
  | `List values -> List.map (string path field) values
  | _ -> fail path (Printf.sprintf "field %S must be an array of strings" field)

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

let compiler_flags path field = function
  | `List values ->
    values
    |> List.concat_map (function
      | `String value ->
        String.split_on_char ' ' value |> List.filter (( <> ) "")
      | `List values ->
        values
        |> List.concat_map (fun value ->
            string path field value |> String.split_on_char ' '
            |> List.filter (( <> ) ""))
      | _ ->
        fail path
          (Printf.sprintf "field %S entries must be strings or arrays" field))
  | _ -> fail path (Printf.sprintf "field %S must be an array" field)

let dependency_name path = function
  | `String value -> {name = value; features = None}
  | `Assoc fields -> (
    reject_duplicate_fields path "dependency" ["name"; "features"] fields;
    match member "name" fields with
    | Some value ->
      let features =
        match optional_member "features" fields with
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
  match (member modern fields, member legacy fields) with
  | Some _, Some _ ->
    fail path
      (Printf.sprintf "fields %S and %S cannot both be set" modern legacy)
  | Some _, None -> parse_dependencies path modern fields
  | None, Some _ -> parse_dependencies path legacy fields
  | None, None -> []

let rec sources_of_json path inherited_dir forced_dev inherited_feature =
  function
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
    reject_duplicate_fields path "source"
      ["dir"; "subdirs"; "type"; "feature"]
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
    let is_dev = Option.value forced_dev ~default:declared_dev in
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
    "path";
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
      ["module"; "in-source"; "suffix"]
      fields;
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

let gentype_args path configured_suffix package_specs_value dependencies =
  function
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
      | Some (`String (("esmodule" | "commonjs") as value)) ->
        ["-bs-gentype-module"; value]
      | Some _ ->
        fail path
          "field \"gentypeconfig.module\" must be \"esmodule\" or \"commonjs\""
    in
    let module_resolution =
      match optional_member "moduleResolution" fields with
      | None -> []
      | Some (`String (("node" | "node16" | "bundler") as value)) ->
        ["-bs-gentype-module-resolution"; value]
      | Some _ ->
        fail path "field \"gentypeconfig.moduleResolution\" is invalid"
    in
    let export_interfaces =
      match optional_member "exportInterfaces" fields with
      | None | Some (`Bool false) -> []
      | Some (`Bool true) -> ["-bs-gentype-export-interfaces"]
      | Some _ ->
        fail path "field \"gentypeconfig.exportInterfaces\" must be a boolean"
    in
    let generated_extension =
      match optional_member "generatedFileExtension" fields with
      | None -> []
      | Some value ->
        [
          "-bs-gentype-generated-extension";
          string path "gentypeconfig.generatedFileExtension" value;
        ]
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
      List.iter
        (fun (from_, target) -> Hashtbl.replace by_source from_ target)
        pairs;
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
    @ List.concat_map
        (fun (dependency : dependency) -> ["-bs-gentype-dep"; dependency.name])
        dependencies
  | _ -> fail path "field \"gentypeconfig\" must be an object"
