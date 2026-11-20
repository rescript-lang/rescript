open Lsp
open Types

let%expect_test "resolves module documentation from an actual completion item" =
  Test.with_project ~name:"completion-resolve" ~text:{|let x = 1
|}
    ~files:
      [
        ( "ModuleStuff.res",
          {|/*** This is a top level module doc. */
let x = 1
|} );
      ]
    (fun ~root ~doc client ->
      Test.change_document client doc ~version:1 ~text:{|let x = 1
ModuleStu|};
      let items =
        match Test.Client.request client (Completion.completion doc (1, 9)) with
        | Some (`List items) -> items
        | Some (`CompletionList list) -> list.items
        | None -> failwith "Expected module completion"
      in
      let item =
        List.find
          (fun (item : CompletionItem.t) -> item.label = "ModuleStuff")
          items
      in
      Test.snapshot ~root client (Client_request.CompletionItemResolve item));
  [%expect
    {|
    {
      "data": { "modulePath": "ModuleStuff", "filePath": "/workspace/Test.res" },
      "detail": "module ModuleStuff",
      "documentation": {
        "kind": "markdown",
        "value": " This is a top level module doc. "
      },
      "kind": 9,
      "label": "ModuleStuff",
      "tags": []
    }
    |}]

let%expect_test "keeps an item without resolve metadata unchanged" =
  Test.with_project ~name:"completion-resolve" ~text:{|let x = 1
|}
    (fun ~root ~doc:_ client ->
      Test.snapshot ~root client
        (Client_request.CompletionItemResolve
           (CompletionItem.create ~label:"x" ())));
  [%expect {| { "label": "x" } |}]

let%expect_test "preserves documentation already supplied on an item" =
  Test.with_project ~name:"completion-resolve" ~text:{|let x = 1
|}
    (fun ~root ~doc:_ client ->
      Test.snapshot ~root client
        (Client_request.CompletionItemResolve
           (CompletionItem.create ~label:"x"
              ~documentation:(`String "Existing docs") ())));
  [%expect {| { "documentation": "Existing docs", "label": "x" } |}]
