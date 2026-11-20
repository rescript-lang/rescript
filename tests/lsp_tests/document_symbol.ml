open Lsp
open Types

let symbols doc =
  Client_request.DocumentSymbol
    (DocumentSymbolParams.create ~textDocument:(Test.text_document doc) ())

let%expect_test "lists nested module types and functions with their ranges" =
  Test.with_project ~name:"document-symbol" ~run_build:false
    ~text:
      {|module ForAuto = {
  type t = int
  let abc = (x: t, _y: int) => x
}
let fa: ForAuto.t = 34
|}
    (fun ~root ~doc client -> Test.snapshot ~root client (symbols doc));
  [%expect
    {|
    [
      {
        "kind": 16,
        "name": "fa",
        "range": {
          "end": { "character": 22, "line": 4 },
          "start": { "character": 0, "line": 4 }
        },
        "selectionRange": {
          "end": { "character": 22, "line": 4 },
          "start": { "character": 0, "line": 4 }
        }
      },
      {
        "children": [
          {
            "kind": 12,
            "name": "abc",
            "range": {
              "end": { "character": 32, "line": 2 },
              "start": { "character": 2, "line": 2 }
            },
            "selectionRange": {
              "end": { "character": 32, "line": 2 },
              "start": { "character": 2, "line": 2 }
            }
          },
          {
            "kind": 26,
            "name": "t",
            "range": {
              "end": { "character": 14, "line": 1 },
              "start": { "character": 2, "line": 1 }
            },
            "selectionRange": {
              "end": { "character": 14, "line": 1 },
              "start": { "character": 2, "line": 1 }
            }
          }
        ],
        "kind": 2,
        "name": "ForAuto",
        "range": {
          "end": { "character": 1, "line": 3 },
          "start": { "character": 7, "line": 0 }
        },
        "selectionRange": {
          "end": { "character": 1, "line": 3 },
          "start": { "character": 7, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "updates symbols from the unsaved document" =
  Test.with_project ~name:"document-symbol" ~run_build:false
    ~text:{|let zzz = 11
|} (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:{|let bracedFunction = {x => x + 1}
|};
      Test.snapshot ~root client (symbols doc));
  [%expect
    {|
    [
      {
        "kind": 12,
        "name": "bracedFunction",
        "range": {
          "end": { "character": 33, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "selectionRange": {
          "end": { "character": 33, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "returns no symbols for an empty document" =
  Test.with_project ~name:"document-symbol" ~run_build:false ~text:{||}
    (fun ~root ~doc client -> Test.snapshot ~root client (symbols doc));
  [%expect {| [] |}]
