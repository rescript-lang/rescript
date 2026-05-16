open Lsp.Types

let create ~(position : Position.t) ~(uri : DocumentUri.t)
    (server : State.t Server.t) =
  let path = DocumentUri.to_path uri in
  let pos = (position.line, position.character) in

  (* NOTE: Should be a config *)
  let supportsMarkdownLinks = true in

  let result =
    let open Analysis in
    let source = (Document_store.get_document ~uri server.state.store).text in
    let debug = false in

    let kindFile = Files.classifySourceFile path in
    let full = Cmt.loadFullCmtFromPath ~path in

    Commands.hover ~source ~kindFile ~pos ~debug ~supportsMarkdownLinks ~full
  in

  match result with
  | None -> None
  | Some value ->
    Some
      (Hover.create
         ~contents:
           (`MarkupContent
              (MarkupContent.create ~kind:MarkupKind.Markdown ~value))
         ())
