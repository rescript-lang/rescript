open Lsp
open Types

let actions doc start end_ =
  Client_request.CodeAction
    (CodeActionParams.create ~textDocument:(Test.text_document doc)
       ~range:(Test.range start end_)
       ~context:(CodeActionContext.create ~diagnostics:[] ())
       ())

let%expect_test "expands a wildcard to the remaining variant cases" =
  Test.with_project ~name:"code-action"
    ~text:
      {|type kind = First | Second | Third | Fourth(int)
let variant = First
let value = switch variant {
| First => "first"
| _ => "other"
}
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (actions doc (4, 3) (4, 3)));
  [%expect
    {|
    [
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "Second | Third | Fourth(_)",
                  "range": {
                    "end": { "character": 3, "line": 4 },
                    "start": { "character": 2, "line": 4 }
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
        "title": "Expand catch-all"
      }
    ]
    |}]

let%expect_test "extracts a local module into its own file" =
  Test.with_project ~name:"code-action"
    ~text:
      {|module ExtractableModule = {
  /** Doc comment. */
  type t = int
  let doStuff = a => a + 1
}
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (actions doc (3, 5) (3, 5)));
  [%expect
    {|
    [
      {
        "edit": {
          "documentChanges": [
            {
              "kind": "create",
              "options": { "ignoreIfExists": true, "overwrite": false },
              "uri": "file:///workspace/ExtractableModule.res"
            },
            {
              "edits": [
                {
                  "newText": "/** Doc comment. */\ntype t = int\nlet doStuff = a => a + 1\n",
                  "range": {
                    "end": { "character": 1, "line": 4 },
                    "start": { "character": 0, "line": 0 }
                  }
                }
              ],
              "textDocument": {
                "uri": "file:///workspace/ExtractableModule.res"
              }
            },
            {
              "edits": [
                {
                  "newText": "",
                  "range": {
                    "end": { "character": 1, "line": 4 },
                    "start": { "character": 0, "line": 0 }
                  }
                }
              ],
              "textDocument": { "uri": "file:///workspace/Test.res" }
            }
          ]
        },
        "kind": "refactor.rewrite",
        "title": "Extract local module \"ExtractableModule\" to file \"ExtractableModule.res\""
      }
    ]
    |}]

let%expect_test "rewrites an if expression as a switch" =
  Test.with_project ~name:"code-action"
    ~text:
      {|type kind = First | Second
let kind = First
let value = if kind == First {
  "First"
} else {
  "Not First"
}
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (actions doc (2, 18) (2, 18)));
  [%expect
    {|
    [
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "switch kind {\n               | First => failwith(\"TODO\")\n               | Second => failwith(\"TODO\")\n               }",
                  "range": {
                    "end": { "character": 19, "line": 2 },
                    "start": { "character": 15, "line": 2 }
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
        "title": "Exhaustive switch"
      },
      {
        "edit": {
          "documentChanges": [
            {
              "edits": [
                {
                  "newText": "switch kind {\n            | First => \"First\"\n            | _ => \"Not First\"\n            }",
                  "range": {
                    "end": { "character": 1, "line": 6 },
                    "start": { "character": 12, "line": 2 }
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
        "title": "Replace with switch"
      }
    ]
    |}]

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
