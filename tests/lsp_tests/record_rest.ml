let text =
  {|type config = {name: string, version: string}
module SubConfig = {
  type t = {version: string}
}
let getVersion = (config: config) =>
  switch config {
  | {name: _, ...SubConfig.t as rest} =>
    rest.version
  }
let getVersionFromParam = ({name: _, ...SubConfig.t as paramRest}: config) => {
  paramRest.version
}
let {name: _, ...SubConfig.t as localRest} = {name: "v", version: "1"}
let {...SubConfig.t as wholeRest} = {version: "2"}
|}

let%expect_test "finds the declaration of a record-rest binding in a switch" =
  Test.with_project ~name:"record-rest" ~text (fun ~root ~doc client ->
      Test.snapshot ~root client (Definition.definition doc (7, 5)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 36, "line": 6 },
          "start": { "character": 32, "line": 6 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "completes a record-rest parameter from an unsaved prefix" =
  Test.with_project ~name:"record-rest" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:
          {|type config = {name: string, version: string}
module SubConfig = {
  type t = {version: string}
}
let getVersion = (config: config) =>
  switch config {
  | {name: _, ...SubConfig.t as rest} =>
    rest.version
  }
let getVersionFromParam = ({name: _, ...SubConfig.t as paramRest}: config) => {
  param
}
let {name: _, ...SubConfig.t as localRest} = {name: "v", version: "1"}
let {...SubConfig.t as wholeRest} = {version: "2"}
|};
      Test.snapshot ~root client (Completion.completion doc (10, 7)));
  [%expect
    {|
    [
      {
        "detail": "SubConfig.t",
        "documentation": {
          "kind": "markdown",
          "value": "```rescript\ntype t = {version: string}\n```"
        },
        "kind": 12,
        "label": "paramRest",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes a type annotation in a whole-record rest pattern" =
  Test.with_project ~name:"record-rest" ~text (fun ~root ~doc client ->
      Test.snapshot ~root client (Completion.completion doc (13, 18)));
  [%expect
    {|
    [
      {
        "detail": "type t",
        "documentation": {
          "kind": "markdown",
          "value": "```rescript\ntype t = {version: string}\n```"
        },
        "kind": 22,
        "label": "t",
        "tags": []
      }
    ]
    |}]

let%expect_test "shows inferred function and record-rest binding hints" =
  Test.with_project ~name:"record-rest" ~text (fun ~root ~doc client ->
      Inlay_hint.enable client;
      Test.snapshot ~root client (Inlay_hint.hints doc (0, 0) (14, 0)));
  [%expect
    {|
    [
      {
        "kind": 1,
        "label": ": SubConfig.t",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 32, "line": 13 }
      },
      {
        "kind": 1,
        "label": ": SubConfig.t",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 41, "line": 12 }
      },
      {
        "kind": 1,
        "label": ": config => string",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 23, "line": 9 }
      },
      {
        "kind": 1,
        "label": ": config => string",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 14, "line": 4 }
      }
    ]
    |}]

let%expect_test "encodes semantic tokens for typed record-rest patterns" =
  Test.with_project ~name:"record-rest" ~text (fun ~root ~doc client ->
      Test.snapshot ~root client (Semantic_tokens.tokens doc));
  [%expect
    {|
    {
      "data": [
        0, 5, 6, 2, 0, 0, 10, 4, 6, 0, 0, 6, 6, 2, 0, 0, 8, 7, 6, 0, 0, 9, 6, 2,
        0, 1, 7, 9, 4, 0, 1, 7, 1, 2, 0, 0, 5, 7, 6, 0, 0, 9, 6, 2, 0, 2, 4, 10,
        1, 0, 0, 14, 6, 1, 0, 0, 8, 6, 2, 0, 1, 9, 6, 1, 0, 1, 5, 4, 6, 0, 0, 12,
        9, 4, 0, 0, 10, 1, 2, 0, 0, 5, 4, 1, 0, 1, 4, 4, 1, 0, 0, 5, 7, 6, 0, 2,
        4, 19, 1, 0, 0, 24, 4, 6, 0, 0, 12, 9, 4, 0, 0, 10, 1, 2, 0, 0, 5, 9, 1,
        0, 0, 12, 6, 2, 0, 1, 2, 9, 1, 0, 0, 10, 7, 6, 0, 2, 5, 4, 6, 0, 0, 12,
        9, 4, 0, 0, 10, 1, 2, 0, 0, 5, 9, 1, 0, 0, 14, 4, 6, 0, 0, 11, 7, 6, 0,
        1, 8, 9, 4, 0, 0, 10, 1, 2, 0, 0, 5, 9, 1, 0, 0, 14, 7, 6, 0
      ]
    }
    |}]
