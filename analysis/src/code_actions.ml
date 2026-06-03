(* This is the return that's expected when resolving code actions *)

let make ~title ~kind ~uri ~new_text ~range =
  let text_document =
    Lsp.Types.OptionalVersionedTextDocumentIdentifier.create
      ~uri:(Uri.from_string uri) ()
  in
  let edit =
    Lsp.Types.WorkspaceEdit.create
      ~documentChanges:
        [
          `TextDocumentEdit
            (Lsp.Types.TextDocumentEdit.create
               ~edits:
                 [
                   `TextEdit (Lsp.Types.TextEdit.create ~range ~newText:new_text);
                 ]
               ~textDocument:text_document);
        ]
      ()
  in
  Lsp.Types.CodeAction.create ~title ~kind ~edit ()

let make_with_document_changes ~title ~kind ~document_changes =
  let edit =
    Lsp.Types.WorkspaceEdit.create ~documentChanges:document_changes ()
  in
  Lsp.Types.CodeAction.create ~title ~kind ~edit ()
