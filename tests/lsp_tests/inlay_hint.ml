open Lsp
open Types

let hints doc start end_ =
  Client_request.InlayHint
    (InlayHintParams.create ~textDocument:(Test.text_document doc)
       ~range:(Test.range start end_) ())

let enable client =
  Test.configure client
    (`Assoc [("inlayHints", `Assoc [("enable", `Bool true)])])

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
