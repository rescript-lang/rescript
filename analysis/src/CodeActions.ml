(* This is the return that's expected when resolving code actions *)

let make ~title ~kind ~uri ~newText ~range =
  let textDocument =
    Lsp.Types.OptionalVersionedTextDocumentIdentifier.create
      ~uri:(Lsp.Uri.of_string uri) ()
  in
  let edit =
    Lsp.Types.WorkspaceEdit.create
      ~documentChanges:
        [
          `TextDocumentEdit
            (Lsp.Types.TextDocumentEdit.create
               ~edits:[`TextEdit (Lsp.Types.TextEdit.create ~range ~newText)]
               ~textDocument);
        ]
      ()
  in
  Lsp.Types.CodeAction.create ~title ~kind ~edit ()

let makeWithDocumentChanges ~title ~kind ~documentChanges =
  let edit = Lsp.Types.WorkspaceEdit.create ~documentChanges () in
  Lsp.Types.CodeAction.create ~title ~kind ~edit ()
