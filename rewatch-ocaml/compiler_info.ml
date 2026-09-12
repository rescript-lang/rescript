type context = {
  build_root: string;
  bsc_path: string;
  bsc_hash: string;
  runtime_path: string;
  source_map_args: string list;
  inherited_compiler_args: string list;
  package_output_specs: package_output_spec list;
}

and package_output_spec = {
  module_format: Config.module_format;
  in_source: bool;
  suffix: string;
}

let format_version = "4"

let package_output_specs (config : Config.t) =
  List.map
    (fun (spec : Config.package_spec) ->
      {
        module_format = spec.module_format;
        in_source = spec.in_source;
        suffix = Config.package_spec_suffix config spec;
      })
    config.package_specs

let make_context ~build_root ~bsc_path ~runtime_path ~source_map_args
    ~inherited_compiler_args ~package_output_specs =
  {
    build_root;
    bsc_path;
    bsc_hash = Digest.file bsc_path |> Digest.to_hex;
    runtime_path;
    source_map_args;
    inherited_compiler_args;
    package_output_specs;
  }

let for_package context ~build_root config =
  {context with build_root; package_output_specs = package_output_specs config}

let path root = File_util.path_of_parts root ["lib"; "bs"; "compiler-info.json"]

let package_output_spec_json spec =
  `Assoc
    [
      ("module", `String (Config.module_format_name spec.module_format));
      ("in_source", `Bool spec.in_source);
      ("suffix", `String spec.suffix);
    ]

let package_output_spec_of_json = function
  | `Assoc fields -> (
    match
      ( List.assoc_opt "module" fields,
        List.assoc_opt "in_source" fields,
        List.assoc_opt "suffix" fields )
    with
    | ( Some (`String module_format),
        Some (`Bool in_source),
        Some (`String suffix) ) -> (
      match module_format with
      | "esmodule" -> Some {module_format = Config.Esmodule; in_source; suffix}
      | "commonjs" -> Some {module_format = Config.Commonjs; in_source; suffix}
      | _ -> None)
    | _ -> None)
  | _ -> None

let package_output_specs_of_json = function
  | `Assoc fields -> (
    match List.assoc_opt "package_output_specs" fields with
    | Some (`List values) ->
      let specs = List.filter_map package_output_spec_of_json values in
      if List.length specs = List.length values then Some specs else None
    | _ -> None)
  | _ -> None

let build_root_of_json = function
  | `Assoc fields -> (
    match List.assoc_opt "build_root" fields with
    | Some (`String build_root) -> Some build_root
    | _ -> None)
  | _ -> None

let read config =
  try Some (Yojson.Safe.from_file (path config.Config.root))
  with Yojson.Json_error _ | Sys_error _ -> None

let json context (config : Config.t) =
  `Assoc
    [
      ("version", `String format_version);
      ("build_root", `String context.build_root);
      ("bsc_path", `String context.bsc_path);
      ("bsc_hash", `String context.bsc_hash);
      ("rescript_config_hash", `String config.file_hash);
      ( "source_map_args",
        `List (List.map (fun value -> `String value) context.source_map_args) );
      ( "inherited_compiler_args",
        `List
          (List.map
             (fun value -> `String value)
             context.inherited_compiler_args) );
      ( "package_output_specs",
        `List (List.map package_output_spec_json context.package_output_specs)
      );
      ("runtime_path", `String context.runtime_path);
    ]

let same_path left right =
  Platform.normalize_path_for_comparison left
  = Platform.normalize_path_for_comparison right

let owns_outputs (config : Config.t) =
  match Option.bind (read config) build_root_of_json with
  | Some build_root -> same_path build_root config.root
  | None -> false

let matches_json context config contents = contents = json context config

let matches context config =
  match read config with
  | Some contents -> matches_json context config contents
  | None -> false

let changed_package_output_specs context config =
  match read config with
  | None -> None
  | Some contents ->
    Option.bind (package_output_specs_of_json contents) (fun previous ->
        if previous = context.package_output_specs then None else Some previous)

let config_with_package_output_specs (config : Config.t) specs =
  let package_specs =
    List.map
      (fun spec : Config.package_spec ->
        {
          module_format = spec.module_format;
          in_source = spec.in_source;
          suffix = Some spec.suffix;
        })
      specs
  in
  {config with package_specs}

let previous_build_exists root =
  File_util.exists
    (File_util.path_of_parts root ["lib"; "ocaml"; ".compiler.log"])

let needs_clean context (config : Config.t) =
  let info_path = path config.root in
  if File_util.exists info_path then not (matches context config)
  else previous_build_exists config.root

let clean_package (config : Config.t) =
  File_util.remove_tree (Build_artifacts.lib_path config.root "bs");
  File_util.remove_tree (Build_artifacts.lib_path config.root "ocaml")

let write_package context (config : Config.t) =
  if not (matches context config) then
    let info_path = path config.root in
    let contents = Yojson.Safe.pretty_to_string (json context config) ^ "\n" in
    File_util.write_file_atomic ~perm:0o644 info_path contents
