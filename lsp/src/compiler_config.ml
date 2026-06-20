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

  type t = {
    name: string;
    namespace: namespace option; [@default None]
    package_specs: package_specs option; [@key "package-specs"] [@default None]
    suffix: string option; [@default None]
  }
  [@@deriving yojson {strict = false}]

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
      package_specs=Some({ "in-source": true, "module": "esmodule" })
      suffix=.jsx
      |}]
end

module Uri_map = Map.Make (Lsp.Uri)

type t = Parse.t Uri_map.t

let parse ~root ~fs =
  let ( /+ ) = Filename.concat in
  let rescript_json = root /+ "rescript.json" in
  match Fs.load ~fs rescript_json with
  | Some content -> (
    match Yojson.Safe.from_string content with
    | json -> (
      match Parse.of_yojson json with
      | Ok s -> Ok s
      | Error _ -> Error ("Failed to parse rescript.json at " ^ rescript_json))
    | exception _ -> Error ("Failed to parse rescript.json at " ^ rescript_json)
    )
  | None -> Error ("Failed to read rescript.json file at " ^ rescript_json)

(* TODO: Rename this to describe the derived output, for example
   get_output_suffix_and_module_folder. It returns both the emitted JS suffix
   and the build folder implied by package-specs. *)
let get_suffix_and_folder (config : Parse.t) =
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
      ( config.suffix |> Option.value ~default:default_suffix,
        folder_name Parse.Commonjs,
        default_in_source )
    | Module_format module_ :: _ ->
      ( config.suffix |> Option.value ~default:default_suffix,
        folder_name module_,
        default_in_source )
    | Module_format_object {module_; suffix; in_source} :: _ ->
      let suffix =
        match (suffix, config.suffix) with
        | Some s, _ -> s
        | None, Some s -> s
        | _ -> default_suffix
      in
      ( suffix,
        folder_name module_,
        in_source |> Option.value ~default:default_in_source ))
  | None ->
    ( config.suffix |> Option.value ~default:default_suffix,
      folder_name Parse.Commonjs,
      default_in_source )
