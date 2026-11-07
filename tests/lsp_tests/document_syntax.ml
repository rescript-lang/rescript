open Lsp
open Types

let diagnostics doc =
  Client_request.TextDocumentDiagnostic
    (DocumentDiagnosticParams.create ~textDocument:(Test.text_document doc) ())

let%expect_test "reports parser errors without requiring a successful build" =
  Test.with_project ~name:"document-syntax" ~run_build:false
    ~text:{|let = 1 + 1.0
let add = =2
lett a = 2
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (diagnostics doc));
  [%expect
    {|
    {
      "kind": "full",
      "items": [
        {
          "message": "Consecutive statements on a line must be separated by ';' or a newline",
          "range": {
            "end": { "character": 6, "line": 2 },
            "start": { "character": 4, "line": 2 }
          },
          "severity": 1,
          "source": "ReScript"
        },
        {
          "message": "This let-binding misses an expression",
          "range": {
            "end": { "character": 11, "line": 1 },
            "start": { "character": 9, "line": 1 }
          },
          "severity": 1,
          "source": "ReScript"
        },
        {
          "message": "I was expecting a name for this let-binding. Example: `let message = \"hello\"`",
          "range": {
            "end": { "character": 5, "line": 0 },
            "start": { "character": 4, "line": 0 }
          },
          "severity": 1,
          "source": "ReScript"
        }
      ]
    }
    |}]

let%expect_test "clears syntax diagnostics after correcting the unsaved buffer"
    =
  Test.with_project ~name:"document-syntax" ~run_build:false
    ~text:{|let add = =2
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (diagnostics doc);
      Test.change_document client doc ~version:1 ~text:{|let add = 2
|};
      Test.snapshot ~root client (diagnostics doc));
  [%expect
    {|
    {
      "kind": "full",
      "items": [
        {
          "message": "This let-binding misses an expression",
          "range": {
            "end": { "character": 11, "line": 0 },
            "start": { "character": 9, "line": 0 }
          },
          "severity": 1,
          "source": "ReScript"
        }
      ]
    }
    { "kind": "full", "items": [] }
    |}]

let%expect_test "reports syntax errors introduced after opening valid code" =
  Test.with_project ~name:"document-syntax" ~run_build:false
    ~text:{|let add = 2
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (diagnostics doc);
      Test.change_document client doc ~version:1 ~text:{|let add = =2
|};
      Test.snapshot ~root client (diagnostics doc));
  [%expect
    {|
    { "kind": "full", "items": [] }
    {
      "kind": "full",
      "items": [
        {
          "message": "This let-binding misses an expression",
          "range": {
            "end": { "character": 11, "line": 0 },
            "start": { "character": 9, "line": 0 }
          },
          "severity": 1,
          "source": "ReScript"
        }
      ]
    }
    |}]
