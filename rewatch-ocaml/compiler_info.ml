type context = {
  bsc_path: string;
  bsc_hash: string;
  runtime_path: string;
  source_map_args: string list;
  package_output_specs: package_output_spec list;
}

and package_output_spec = {
  module_format: string;
  in_source: bool;
  suffix: string;
}

let format_version = "2"

let package_output_specs (config : Config.t) =
  List.map
    (fun (spec : Config.package_spec) ->
      {
        module_format = Config.module_format_name spec.module_format;
        in_source = spec.in_source;
        suffix = Config.package_spec_suffix config spec;
      })
    config.package_specs

let make_context ~bsc_path ~runtime_path ~source_map_args ~package_output_specs =
  {
    bsc_path;
    bsc_hash = Digest.file bsc_path |> Digest.to_hex;
    runtime_path;
    source_map_args;
    package_output_specs;
  }

let path root =
  Build_artifacts.path_of_parts root ["lib"; "bs"; "compiler-info.json"]

let config_hash (config : Config.t) =
  Digest.file config.path |> Digest.to_hex

let package_output_spec_json spec =
  `Assoc
    [
      ("module", `String spec.module_format);
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
    | ( Some (`String ("esmodule" | "commonjs" as module_format)),
        Some (`Bool in_source),
        Some (`String suffix) ) ->
      Some {module_format; in_source; suffix}
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

let read config =
  try Some (Yojson.Safe.from_file (path config.Config.root))
  with Yojson.Json_error _ | Sys_error _ -> None

let json context (config : Config.t) =
  `Assoc
    [
      ("version", `String format_version);
      ("bsc_path", `String context.bsc_path);
      ("bsc_hash", `String context.bsc_hash);
      ("rescript_config_hash", `String (config_hash config));
      ( "source_map_args",
        `List (List.map (fun value -> `String value) context.source_map_args) );
      ( "package_output_specs",
        `List (List.map package_output_spec_json context.package_output_specs) );
      ("runtime_path", `String context.runtime_path);
    ]

let matches context config =
  read config = Some (json context config)

let changed_package_output_specs context config =
  let previous = Option.bind (read config) package_output_specs_of_json in
  Option.bind previous (fun previous ->
      if previous = context.package_output_specs then None else Some previous)

let config_with_package_output_specs (config : Config.t) specs =
  let package_specs =
    List.filter_map
      (fun spec ->
        let module_format =
          match spec.module_format with
          | "esmodule" -> Some Config.Esmodule
          | "commonjs" -> Some Config.Commonjs
          | _ -> None
        in
        Option.map
          (fun module_format : Config.package_spec ->
            {module_format; in_source = spec.in_source; suffix = Some spec.suffix})
          module_format)
      specs
  in
  {config with package_specs}

let previous_build_exists root =
  Sys.file_exists
    (Build_artifacts.path_of_parts root ["lib"; "ocaml"; ".compiler.log"])

let needs_clean context (config : Config.t) =
  let info_path = path config.root in
  if Sys.file_exists info_path then not (matches context config)
  else previous_build_exists config.root

let clean_package (config : Config.t) =
  Build_artifacts.remove_tree (Build_artifacts.lib_path config.root "bs");
  Build_artifacts.remove_tree (Build_artifacts.lib_path config.root "ocaml")

let verify_package context config =
  let should_clean = needs_clean context config in
  if should_clean then clean_package config;
  should_clean

let write_package context (config : Config.t) =
  if not (matches context config) then (
    let info_path = path config.root in
    Build_artifacts.ensure_dir (Filename.dirname info_path);
    let temporary =
      Filename.temp_file ~temp_dir:(Filename.dirname info_path)
        ".compiler-info-" ".json.tmp"
    in
    Fun.protect
      ~finally:(fun () -> Build_artifacts.remove_file temporary)
      (fun () ->
        let channel = open_out_bin temporary in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            Yojson.Safe.pretty_to_channel channel (json context config);
            output_char channel '\n');
        Build_artifacts.remove_file info_path;
        Sys.rename temporary info_path))
