open Lsp
open Types

let large_document ~name =
  "/*"
  ^ String.make ((2 * 1024 * 1024) + 137) 'x'
  ^ "*/\nlet " ^ name ^ " = 1\n"

let print_symbol_names client doc =
  match
    Test.Client.request client
      (Client_request.DocumentSymbol
         (DocumentSymbolParams.create ~textDocument:(Test.text_document doc) ()))
  with
  | Some (`DocumentSymbol symbols) ->
    List.iter
      (fun (symbol : DocumentSymbol.t) -> print_endline symbol.name)
      symbols
  | _ -> failwith "Expected document symbols"

let%expect_test
    "opens a document larger than the input buffer and keeps reading" =
  Test.with_project ~name:"large-document-open" ~run_build:false
    ~text:(large_document ~name:"opened") (fun ~root:_ ~doc client ->
      print_symbol_names client doc;
      Test.change_document client doc ~version:1 ~text:{|let after_open = 2|};
      print_symbol_names client doc);
  [%expect {|
    opened
    after_open
    |}]

let%expect_test
    "applies a change larger than the input buffer and keeps reading" =
  Test.with_project ~name:"large-document-change" ~run_build:false
    ~text:{|let original = 1|} (fun ~root:_ ~doc client ->
      print_symbol_names client doc;
      Test.change_document client doc ~version:1
        ~text:(large_document ~name:"changed");
      print_symbol_names client doc;
      Test.change_document client doc ~version:2 ~text:{|let after_change = 2|};
      print_symbol_names client doc);
  [%expect {|
    original
    changed
    after_change
    |}]

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
