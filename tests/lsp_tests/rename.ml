open Lsp
open Types

let rename doc pos new_name =
  Client_request.TextDocumentRename
    (RenameParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ~newName:new_name ())

let%expect_test "renames a value and all local uses" =
  Test.with_project ~name:"rename" ~text:{|let x = 12
let a = x
let c = x
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 4) "y"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 5, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            },
            {
              "newText": "y",
              "range": {
                "end": { "character": 9, "line": 1 },
                "start": { "character": 8, "line": 1 }
              }
            },
            {
              "newText": "y",
              "range": {
                "end": { "character": 9, "line": 2 },
                "start": { "character": 8, "line": 2 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

let%expect_test "renames a labeled parameter and its use" =
  Test.with_project ~name:"rename" ~text:{|let foo = (~xx) => xx + 1
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 19) "yy"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "yy",
              "range": {
                "end": { "character": 14, "line": 0 },
                "start": { "character": 11, "line": 0 }
              }
            },
            {
              "newText": "yy",
              "range": {
                "end": { "character": 21, "line": 0 },
                "start": { "character": 19, "line": 0 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

let%expect_test "renames an export in its interface and consumer" =
  Test.with_project ~name:"rename" ~text:{|let x = 12
|}
    ~files:
      [("Test.resi", {|let x: int
|}); ("Consumer.res", {|let a = Test.x
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 4) "y"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 5, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        },
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 5, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            }
          ],
          "textDocument": {
            "uri": "file:///workspace/./Test.resi",
            "version": 0
          }
        },
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 14, "line": 0 },
                "start": { "character": 13, "line": 0 }
              }
            }
          ],
          "textDocument": {
            "uri": "file:///workspace/./Consumer.res",
            "version": 0
          }
        }
      ]
    }
    |}]
