module Parse = struct
  type namespace = Namespace_bool of bool | Namespace_string of string

  let namespace_to_yojson = function
    | Namespace_bool value -> `Bool value
    | Namespace_string value -> `String value

  let namespace_of_yojson = function
    | `Bool value -> Ok (Namespace_bool value)
    | `String value -> Ok (Namespace_string value)
    | json ->
      Error
        ("Expected namespace to be a boolean or string, got "
       ^ Yojson.Safe.to_string json)

  type module_format = Commonjs | Es6 | Es6_global | Esmodule

  let module_format_to_yojson = function
    | Commonjs -> `String "commonjs"
    | Es6 -> `String "es6"
    | Es6_global -> `String "es6-global"
    | Esmodule -> `String "esmodule"

  let module_format_of_yojson = function
    | `String "commonjs" -> Ok Commonjs
    | `String "es6" -> Ok Es6
    | `String "es6-global" -> Ok Es6_global
    | `String "esmodule" -> Ok Esmodule
    | _ -> Error "Compiler_config.module_format"

  type module_format_object = {
    in_source: bool option; [@key "in-source"] [@default None]
    module_: module_format; [@key "module"]
    suffix: string option; [@default None]
  }
  [@@deriving yojson {strict = false}]

  type package_spec =
    | Module_format of module_format
    | Module_format_object of module_format_object

  let package_spec_to_yojson = function
    | Module_format module_format -> module_format_to_yojson module_format
    | Module_format_object module_format_object ->
      module_format_object_to_yojson module_format_object

  let package_spec_of_yojson json =
    match module_format_of_yojson json with
    | Ok module_format -> Ok (Module_format module_format)
    | Error _ -> (
      match module_format_object_of_yojson json with
      | Ok module_format_object ->
        Ok (Module_format_object module_format_object)
      | Error message -> Error message)

  type package_specs = package_spec list

  let package_specs_to_yojson package_specs =
    `List (List.map package_spec_to_yojson package_specs)

  let package_specs_of_yojson json =
    let rec collect = function
      | [] -> Ok []
      | json :: rest -> (
        match package_spec_of_yojson json with
        | Ok package_spec -> (
          match collect rest with
          | Ok package_specs -> Ok (package_spec :: package_specs)
          | Error _ as error -> error)
        | Error _ as error -> error)
    in

    match json with
    | `List package_specs -> collect package_specs
    | json -> (
      match package_spec_of_yojson json with
      | Ok package_spec -> Ok [package_spec]
      | Error _ as error -> error)

  type dependencie_object = {
    name: string;
    features: string list option; [@default None]
  }
  [@@deriving yojson {strict = false}]

  type dependencies = String of string | Object of dependencie_object

  let dependencies_to_yojson = function
    | String name -> `String name
    | Object dependency -> dependencie_object_to_yojson dependency

  let dependencies_of_yojson = function
    | `String name -> Ok (String name)
    | `Assoc _ as json -> (
      match dependencie_object_of_yojson json with
      | Ok dependency -> Ok (Object dependency)
      | Error message -> Error message)
    | _ -> Error "Expected a dependency name or object"

  type t = {
    name: string;
    namespace: namespace option; [@default None]
    package_specs: package_specs option; [@key "package-specs"] [@default None]
    suffix: string option; [@default None]
    dependencies: dependencies list option; [@default None]
    dev_dependencies: dependencies list option;
        [@key "dev-dependencies"] [@default None]
    (* Old fields, deprecated *)
    bs_dependencies: dependencies list option;
        [@key "bs-dependencies"] [@default None]
    bs_dev_dependencies: dependencies list option;
        [@key "bs-dev-dependencies"] [@default None]
  }
  [@@deriving yojson {strict = false}]

  let%expect_test "namespace codecs accept the compiler's JSON forms" =
    List.iter
      (fun json ->
        match namespace_of_yojson json with
        | Ok namespace ->
          namespace_to_yojson namespace
          |> Yojson.Safe.to_string |> print_endline
        | Error message -> print_endline message)
      [`Bool true; `Bool false; `String "App"; `Int 1];
    [%expect
      {|
      true
      false
      "App"
      Expected namespace to be a boolean or string, got 1
      |}]

  let%expect_test "package specs codecs accept single values and lists" =
    List.iter
      (fun text ->
        match package_specs_of_yojson (Yojson.Safe.from_string text) with
        | Ok specs ->
          package_specs_to_yojson specs
          |> Yojson.Safe.to_string |> print_endline
        | Error message -> print_endline message)
      [
        {|"commonjs"|};
        {|["es6","es6-global","esmodule"]|};
        {|{"module":"esmodule","in-source":true,"suffix":".mjs"}|};
        {|[]|};
        {|["commonjs",42]|};
      ];
    [%expect
      {|
      ["commonjs"]
      ["es6","es6-global","esmodule"]
      [{"in-source":true,"module":"esmodule","suffix":".mjs"}]
      []
      Compiler_config.Parse.module_format_object
      |}]

  let%expect_test "parse build schema" =
    let prin_config config =
      match config with
      | Error message -> print_endline ("Error: " ^ message)
      | Ok config -> (
        Printf.printf "name=%s\n" config.name;
        (match config.namespace with
        | Some (Namespace_string namespace) ->
          Printf.printf "namespace=%s\n" namespace
        | Some (Namespace_bool namespace) ->
          Printf.printf "namespace=%b\n" namespace
        | None -> print_endline "namespace=None");
        (match config.dependencies with
        | None -> print_endline "dependencies=None"
        | Some deps ->
          let deps_names =
            deps
            |> List.map (fun dep ->
                match dep with
                | String name -> name
                | Object {name} -> name)
            |> String.concat ", "
          in
          Printf.printf "dependencies=Some(%s)\n" deps_names);
        (match config.package_specs with
        | Some specs ->
          let r =
            specs
            |> List.map (fun spec ->
                package_spec_to_yojson spec |> Yojson.Safe.pretty_to_string)
            |> String.concat ", "
          in
          Printf.sprintf "package_specs=Some(%s)" r |> print_endline
        | None -> print_endline "package_specs=None");
        match config.suffix with
        | Some suffix -> Printf.sprintf "suffix=%s" suffix |> print_endline
        | None -> print_endline "suffix=None")
    in

    let json =
      Yojson.Safe.from_string
        {|{
        "name": "app",
        "namespace": "App",
        "dependencies": [],
        "package-specs": [
          "esmodule",
          {
            "in-source": true,
            "module": "commonjs",
            "suffix": ".cjs"
          }
        ],
        "suffix": ".js"
      }|}
    in
    prin_config (of_yojson json);
    [%expect
      {|
      name=app
      namespace=App
      dependencies=Some()
      package_specs=Some("esmodule", { "in-source": true, "module": "commonjs", "suffix": ".cjs" })
      suffix=.js
      |}];

    let json_2 =
      Yojson.Safe.from_string
        {|{
            "name": "@rescript-lang/guide",
            "namespace": false,
            "dependencies": [
              "@rescript-lang/playground",
              "@rescript-lang/shared",
              "@rescript/react",
              "@rescript/webapi"
            ],
            "compiler-flags": ["-open WebAPI.Global"],
            "sources": [
              {
                "dir": "__tests__",
                "subdirs": true,
                "type": "dev"
              },
              {
                "dir": "app",
                "subdirs": true
              }
            ],
            "warnings": {
              "error": "+8"
            }
          }|}
    in
    prin_config (of_yojson json_2);
    [%expect
      {|
      name=@rescript-lang/guide
      namespace=false
      dependencies=Some(@rescript-lang/playground, @rescript-lang/shared, @rescript/react, @rescript/webapi)
      package_specs=None
      suffix=None
      |}];

    let json_3 =
      Yojson.Safe.from_string
        {|{
            "name": "rescript-lang.org-monorepo",
            "dependencies": [
              "@rescript-lang/shared",
              "@rescript-lang/playground",
              "@rescript-lang/guide",
              "@rescript-lang/docs"
            ],
            "sources": [],
            "jsx": {
              "preserve": true,
              "version": 4
            },
            "package-specs": {
              "module": "esmodule",
              "in-source": true
            },
            "suffix": ".jsx"
          }
|}
    in
    prin_config (of_yojson json_3);
    [%expect
      {|
      name=rescript-lang.org-monorepo
      namespace=None
      dependencies=Some(@rescript-lang/shared, @rescript-lang/playground, @rescript-lang/guide, @rescript-lang/docs)
      package_specs=Some({ "in-source": true, "module": "esmodule" })
      suffix=.jsx
      |}];

    let json_4 =
      Yojson.Safe.from_string
        {|
{
  "name": "app",
  "dependencies": [
    "@plain/dep",
    { "name": "@example/lib", "features": ["native"] }
  ]
}
|}
    in
    prin_config (of_yojson json_4);
    [%expect
      {|
      name=app
      namespace=None
      dependencies=Some(@plain/dep, @example/lib)
      package_specs=None
      suffix=None
      |}]

  let%expect_test "dependency JSON round trips preserve features" =
    let print json =
      match dependencies_of_yojson (Yojson.Safe.from_string json) with
      | Ok dependency ->
        dependencies_to_yojson dependency
        |> Yojson.Safe.pretty_to_string |> print_endline
      | Error message -> Printf.printf "Error: %s\n" message
    in
    print {|"@plain/dep"|};
    print {|{"name":"@example/lib","features":["native"]}|};
    print {|{"name":"@example/lib"}|};
    print {|42|};
    print {|{"features":["native"]}|};
    print {|{"name":"@example/lib","features":true}|};
    [%expect
      {|
      "@plain/dep"
      { "name": "@example/lib", "features": [ "native" ] }
      { "name": "@example/lib" }
      Error: Expected a dependency name or object
      Error: Compiler_config.Parse.dependencie_object.name
      Error: Compiler_config.Parse.dependencie_object.features
      |}]
end

module Uri_map = Map.Make (Lsp.Uri)

type t = Parse.t Uri_map.t

let parse ~root ~fs =
  let ( /+ ) = Filename.concat in

  let config_file =
    let rescript_json = root /+ Constants.rescript_json in
    let bsconfig_json = root /+ Constants.bsconfig_json in
    if Fs.exists ~follow:false ~fs rescript_json then rescript_json
    else bsconfig_json
  in

  match Fs.load ~fs config_file with
  | Some content -> (
    match Yojson.Safe.from_string content with
    | json -> (
      match Parse.of_yojson json with
      | Ok s -> Ok s
      | Error msg ->
        Error
          (Printf.sprintf "Failed to parse compiler config %s. %s" config_file
             msg))
    | exception exc ->
      Error
        (Printf.sprintf "Failed to parse rescript configuration file at %s. %s"
           root (Printexc.to_string exc)))
  | None -> Error ("Failed to read rescript configuration file at " ^ root)

type output_config = {suffix: string; folder: string; in_source: bool}
let get_output_config (config : Parse.t) =
  let default_suffix = ".js" in
  let default_in_source = false in
  let folder_name = function
    | Parse.Commonjs -> "js"
    | Es6_global -> "es6_global"
    | Es6 | Esmodule -> "es6"
  in
  match config.package_specs with
  | Some pkg_spec -> (
    match pkg_spec with
    | [] ->
      {
        suffix = config.suffix |> Option.value ~default:default_suffix;
        folder = folder_name Parse.Commonjs;
        in_source = default_in_source;
      }
    | Module_format module_ :: _ ->
      {
        suffix = config.suffix |> Option.value ~default:default_suffix;
        folder = folder_name module_;
        in_source = default_in_source;
      }
    | Module_format_object {module_; suffix; in_source} :: _ ->
      let suffix =
        match (suffix, config.suffix) with
        | Some s, _ -> s
        | None, Some s -> s
        | _ -> default_suffix
      in
      {
        suffix;
        folder = folder_name module_;
        in_source = in_source |> Option.value ~default:default_in_source;
      })
  | None ->
    {
      suffix = config.suffix |> Option.value ~default:default_suffix;
      folder = folder_name Parse.Commonjs;
      in_source = default_in_source;
    }

let get_all_dependecies_names (config : Parse.t) =
  let names deps =
    match deps with
    | None -> []
    | Some deps ->
      deps
      |> List.map (fun dep ->
          match dep with
          | Parse.String name -> name
          | Object {name} -> name)
  in
  (config.dependencies |> names)
  @ (config.dev_dependencies |> names)
  @ (config.bs_dependencies |> names)
  @ (config.bs_dev_dependencies |> names)
