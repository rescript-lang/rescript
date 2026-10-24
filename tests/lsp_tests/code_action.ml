open Lsp
open Types

let actions doc start end_ =
  Client_request.CodeAction
    (CodeActionParams.create ~textDocument:(Test.text_document doc)
       ~range:(Test.range start end_)
       ~context:(CodeActionContext.create ~diagnostics:[] ())
       ())

let%expect_test "offers a type annotation for an inferred string binding" =
  Test.with_project ~name:"code-action" ~text:{|let name = "hello"
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (actions doc (0, 5) (0, 5)));
  [%expect
    {|
    [
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": ": string",
                  "range": {
                    "end": { "character": 8, "line": 0 },
                    "start": { "character": 8, "line": 0 }
                  }
                }
              ],
              "textDocument": {
                "uri": "file:///workspace/Test.res",
                "version": 0
              }
            }
          ]
        },
        "kind": "refactor.rewrite",
        "title": "Add type annotation"
      },
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "/**\n\n*/\nlet name = \"hello\"",
                  "range": {
                    "end": { "character": 18, "line": 0 },
                    "start": { "character": 0, "line": 0 }
                  }
                }
              ],
              "textDocument": {
                "uri": "file:///workspace/Test.res",
                "version": 0
              }
            }
          ]
        },
        "kind": "refactor.rewrite",
        "title": "Add Documentation template"
      }
    ]
    |}]

let%expect_test "offers braces around a function body" =
  Test.with_project ~name:"code-action"
    ~text:{|let name = "hello"
let noBraces = () => name
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (actions doc (1, 21) (1, 21)));
  [%expect
    {|
    [
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "let noBraces = () => {\n  name\n}",
                  "range": {
                    "end": { "character": 25, "line": 1 },
                    "start": { "character": 0, "line": 1 }
                  }
                }
              ],
              "textDocument": {
                "uri": "file:///workspace/Test.res",
                "version": 0
              }
            }
          ]
        },
        "kind": "refactor.rewrite",
        "title": "Add braces to function"
      }
    ]
    |}]

let%expect_test "does not offer another annotation for an annotated binding" =
  Test.with_project ~name:"code-action" ~text:{|let annotated: int = 34
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (actions doc (0, 5) (0, 5)));
  [%expect
    {|
    [
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "/**\n\n*/\nlet annotated: int = 34",
                  "range": {
                    "end": { "character": 23, "line": 0 },
                    "start": { "character": 0, "line": 0 }
                  }
                }
              ],
              "textDocument": {
                "uri": "file:///workspace/Test.res",
                "version": 0
              }
            }
          ]
        },
        "kind": "refactor.rewrite",
        "title": "Add Documentation template"
      }
    ]
    |}]
