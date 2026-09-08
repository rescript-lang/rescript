type context = {
  bsc_path: string;
  bsc_hash: string;
  runtime_path: string;
  source_map_args: string list;
}

let format_version = "1"

let make_context ~bsc_path ~runtime_path ~source_map_args =
  {
    bsc_path;
    bsc_hash = Digest.file bsc_path |> Digest.to_hex;
    runtime_path;
    source_map_args;
  }

let path root =
  Build_artifacts.path_of_parts root ["lib"; "bs"; "compiler-info.json"]

let config_hash (config : Config.t) =
  Digest.file config.path |> Digest.to_hex

let json context (config : Config.t) =
  `Assoc
    [
      ("version", `String format_version);
      ("bsc_path", `String context.bsc_path);
      ("bsc_hash", `String context.bsc_hash);
      ("rescript_config_hash", `String (config_hash config));
      ( "source_map_args",
        `List (List.map (fun value -> `String value) context.source_map_args) );
      ("runtime_path", `String context.runtime_path);
    ]

let matches context config =
  try Yojson.Safe.from_file (path config.Config.root) = json context config
  with Yojson.Json_error _ | Sys_error _ -> false

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
