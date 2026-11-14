open Lsp
open Types

let%expect_test
    "applies a partial text change before computing document symbols" =
  Test.with_project ~name:"partial-text-change" ~run_build:false
    ~text:{|let value = 1
|} (fun ~root ~doc client ->
      Test.Client.send_notification client
        (Client_notification.TextDocumentDidChange
           (DidChangeTextDocumentParams.create
              ~textDocument:
                (VersionedTextDocumentIdentifier.create ~uri:doc.uri ~version:1)
              ~contentChanges:
                [
                  `TextDocumentContentChangePartial
                    (TextDocumentContentChangePartial.create
                       ~range:(Test.range (0, 4) (0, 9))
                       ~text:"renamed" ());
                ]));
      Test.snapshot ~root client
        (Client_request.DocumentSymbol
           (DocumentSymbolParams.create ~textDocument:(Test.text_document doc)
              ())));
  [%expect
    {|
    [
      {
        "kind": 16,
        "name": "renamed",
        "range": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "selectionRange": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]
