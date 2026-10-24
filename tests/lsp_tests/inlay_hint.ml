open Lsp
open Types

let hints doc start end_ =
  Client_request.InlayHint
    (InlayHintParams.create ~textDocument:(Test.text_document doc)
       ~range:(Test.range start end_) ())

let enable client =
  Test.configure client
    (`Assoc [("inlayHints", `Assoc [("enable", `Bool true)])])

let%expect_test "shows inferred dictionary and dictionary pattern types" =
  Test.with_project ~name:"inlay-hint"
    ~text:
      {|let ints = dict{"a": 1}
let spread = dict{...ints, "b": 2}
let dict{"a": ?first} = ints
|}
    (fun ~root ~doc client ->
      enable client;
      Test.snapshot ~root client (hints doc (0, 0) (3, 0)));
  [%expect
    {|
    [
      {
        "kind": 1,
        "label": ": option<int>",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 20, "line": 2 }
      },
      {
        "kind": 1,
        "label": ": dict<int>",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 10, "line": 1 }
      },
      {
        "kind": 1,
        "label": ": dict<int>",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 8, "line": 0 }
      }
    ]
    |}]

let%expect_test "shows inferred plain and tagged template types" =
  Test.with_project ~name:"inlay-hint"
    ~text:
      {|@module("tag")
external tag: taggedTemplate<string, string> = "default"
let value = "x"
let template = `value: ${value}`
let tagged = tag`value: ${value}`
|}
    (fun ~root ~doc client ->
      enable client;
      Test.snapshot ~root client (hints doc (0, 0) (5, 0)));
  [%expect
    {|
    [
      {
        "kind": 1,
        "label": ": string",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 10, "line": 4 }
      },
      {
        "kind": 1,
        "label": ": string",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 12, "line": 3 }
      },
      {
        "kind": 1,
        "label": ": string",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 9, "line": 2 }
      }
    ]
    |}]

let%expect_test "omits generated React component bindings from inlay hints" =
  Test.with_project ~name:"inlay-hint-jsx" ~jsx:true
    ~text:{|@react.component
let make = (~name) => React.string(name)
|}
    (fun ~root ~doc client ->
      enable client;
      Test.snapshot ~root client (hints doc (0, 0) (2, 0)));
  [%expect {| [] |}]

let%expect_test "does not return inlay hints when disabled" =
  Test.with_project ~name:"inlay-hint" ~text:{|let add = (x, y) => x + y
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (hints doc (0, 0) (1, 0)));
  [%expect {| null |}]

let%expect_test "shows inferred function and destructured record types" =
  Test.with_project ~name:"inlay-hint"
    ~text:
      {|let add = (x, y) => x + y
type foo = {name: string, age: int}
let alice = {name: "Alice", age: 42}
let {name, age} = alice
|}
    (fun ~root ~doc client ->
      enable client;
      Test.snapshot ~root client (hints doc (0, 0) (4, 0)));
  [%expect
    {|
    [
      {
        "kind": 1,
        "label": ": int",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 14, "line": 3 }
      },
      {
        "kind": 1,
        "label": ": string",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 9, "line": 3 }
      },
      {
        "kind": 1,
        "label": ": foo",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 9, "line": 2 }
      },
      {
        "kind": 1,
        "label": ": (int, int) => int",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 7, "line": 0 }
      }
    ]
    |}]

let%expect_test "restricts inlay hints to the requested lines" =
  Test.with_project ~name:"inlay-hint"
    ~text:{|let add = (x, y) => x + y
let withAs = (~xx as yyy) => yyy + 1
|}
    (fun ~root ~doc client ->
      enable client;
      Test.snapshot ~root client (hints doc (1, 0) (1, 35)));
  [%expect
    {|
    [
      {
        "kind": 1,
        "label": ": (~xx: int) => int",
        "paddingLeft": true,
        "paddingRight": false,
        "position": { "character": 10, "line": 1 }
      }
    ]
    |}]
