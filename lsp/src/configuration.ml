type hover = {
  support_markdown_links: bool; [@key "supportMarkdownLinks"] [@default false]
}
[@@deriving yojson {strict = false}]

type inlay_hints = {
  enable: bool; [@default false]
  max_length: int option; [@key "maxLength"] [@default Some 25]
}
[@@deriving yojson {strict = false}]

type signature_help = {
  enable: bool; [@default true]
  for_constructor_payloads: bool; [@key "forConstructorPayloads"] [@default true]
}
[@@deriving yojson {strict = false}]

type t = {
  hover: hover; [@default {support_markdown_links = false}]
  code_lens: bool; [@key "codeLens"] [@default false]
  inlay_hints: inlay_hints;
      [@key "inlayHints"] [@default {enable = false; max_length = Some 25}]
  signature_help: signature_help;
      [@key "signatureHelp"]
      [@default {enable = true; for_constructor_payloads = true}]
}
[@@deriving yojson {strict = false}]

let default =
  {
    hover = {support_markdown_links = false};
    code_lens = false;
    inlay_hints = {enable = false; max_length = Some 25};
    signature_help = {enable = true; for_constructor_payloads = true};
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
