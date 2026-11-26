module Config = Rewatch_ocaml_lib.Config
module Uri_map = Map.Make (Lsp.Uri)

type t = Config.t Uri_map.t

let parse ~root ~fs =
  let ( /+ ) = Filename.concat in
  let config_file =
    let rescript_json = root /+ Constants.rescript_json in
    if Fs.exists ~follow:false ~fs rescript_json then rescript_json
    else root /+ Constants.bsconfig_json
  in
  match Fs.load ~fs config_file with
  | Some content -> (
    try Ok (Config.of_string ~path:config_file ~root content)
    with Config.Error message -> Error message)
  | None -> Error ("Failed to read rescript configuration file at " ^ root)

let get_all_dependecies_names (config : Config.t) =
  List.map
    (fun (dependency : Config.dependency) -> dependency.name)
    (config.dependencies @ config.dev_dependencies)

(* State dumps expose the effective project settings, not a second copy of
   the rescript.json schema or its decoder. *)
let to_yojson (config : Config.t) =
  let optional_string = function
    | None -> `Null
    | Some value -> `String value
  in
  let dependencies values =
    `List
      (List.map
         (fun (dependency : Config.dependency) ->
           `Assoc
             ([("name", `String dependency.name)]
             @
             match dependency.features with
             | None -> []
             | Some features ->
               [
                 ( "features",
                   `List (List.map (fun value -> `String value) features) );
               ]))
         values)
  in
  `Assoc
    [
      ("name", `String config.name);
      ("namespace", optional_string (Config.namespace_name config.namespace));
      ( "namespace-entry",
        optional_string (Config.namespace_entry config.namespace) );
      ("suffix", `String config.suffix);
      ("dependencies", dependencies config.dependencies);
      ("dev-dependencies", dependencies config.dev_dependencies);
      ( "package-specs",
        `List
          (List.map
             (fun (spec : Config.package_spec) ->
               `Assoc
                 [
                   ( "module",
                     `String (Config.module_format_name spec.module_format) );
                   ("in-source", `Bool spec.in_source);
                   ("suffix", `String (Config.package_spec_suffix config spec));
                 ])
             config.package_specs) );
    ]
