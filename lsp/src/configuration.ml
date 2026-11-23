type hover = {
  support_markdown_links: bool; [@key "supportMarkdownLinks"] [@default false]
}
[@@deriving yojson {strict = false}]

type inlay_hints = {
  enable: bool; [@default false]
  max_length: int option; [@key "maxLength"] [@default Some 25]
}
[@@deriving yojson {strict = false}]

type t = {
  hover: hover; [@default {support_markdown_links = false}]
  code_lens: bool; [@key "codeLens"] [@default false]
  inlay_hints: inlay_hints;
      [@key "inlayHints"] [@default {enable = false; max_length = Some 25}]
}
[@@deriving yojson {strict = false}]

let default =
  {
    hover = {support_markdown_links = false};
    code_lens = false;
    inlay_hints = {enable = false; max_length = Some 25};
  }

let%expect_test "parse config" =
  let parse_and_print_config json =
    match of_yojson (Yojson.Safe.from_string json) with
    | Ok c -> c |> to_yojson |> Yojson.Safe.pretty_to_string |> print_endline
    | Error _ -> print_endline "Error"
  in

  let example_config_json_1 =
    {|
  {
    "askToStartBuild": false,
    "inlayHints": { "enable": true, "maxLength": 25 },
    "codeLens": true,
    "signatureHelp": { "enabled": true, "forConstructorPayloads": true },
    "incrementalTypechecking": { "enable": true, "acrossFiles": false },
    "cache": { "projectConfig": { "enable": true } },
    "binaryPath": null,
    "platformPath": null,
    "runtimePath": null,
    "compileStatus": { "enable": true },
    "logLevel": "info"
  }
  |}
  in
  parse_and_print_config example_config_json_1;
  [%expect {| { "codeLens": true, "inlayHints": { "enable": true } } |}]

let%expect_test "settings retain defaults, optional limits, and unknown fields"
    =
  List.iter
    (fun text ->
      match of_yojson (Yojson.Safe.from_string text) with
      | Ok config ->
        let json = to_yojson config in
        assert (of_yojson json = Ok config);
        print_endline (Yojson.Safe.to_string json)
      | Error message -> failwith message)
    [
      {|{}|};
      {|{"hover":{},"inlayHints":{},"futureSetting":true}|};
      {|{"hover":{"supportMarkdownLinks":true},"codeLens":true,"inlayHints":{"enable":true,"maxLength":0}}|};
      {|{"inlayHints":{"maxLength":null}}|};
    ];
  [%expect
    {|
    {}
    {}
    {"hover":{"supportMarkdownLinks":true},"codeLens":true,"inlayHints":{"enable":true,"maxLength":0}}
    {"inlayHints":{"maxLength":null}}
    |}]

let%expect_test "settings reject invalid known fields" =
  List.iter
    (fun text ->
      Printf.printf "%s: %b\n" text
        (Result.is_error (of_yojson (Yojson.Safe.from_string text))))
    [
      {|[]|};
      {|{"hover":null}|};
      {|{"codeLens":null}|};
      {|{"codeLens":1}|};
      {|{"hover":{"supportMarkdownLinks":"true"}}|};
      {|{"inlayHints":false}|};
      {|{"inlayHints":{"enable":null}}|};
      {|{"inlayHints":{"maxLength":"25"}}|};
    ];
  [%expect
    {|
    []: true
    {"hover":null}: true
    {"codeLens":null}: true
    {"codeLens":1}: true
    {"hover":{"supportMarkdownLinks":"true"}}: true
    {"inlayHints":false}: true
    {"inlayHints":{"enable":null}}: true
    {"inlayHints":{"maxLength":"25"}}: true
    |}]
