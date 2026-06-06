open Lsp.Types

let create ~(position : Position.t) ~(uri : DocumentUri.t)
    (server : State.t Server.t) =
  (* TODO: should be a config *)
  let supports_markdown_links = false in

  let source = (Document_store.get_document ~uri server.state.store).text in
  let full =
    Analysis.Cmt.load_full_cmt_from_path ~path:(DocumentUri.to_path uri)
  in

  Analysis.Commands.hover ~source ~kind_file:(Document.kind uri)
    ~pos:(position.line, position.character)
    ~debug:false ~supports_markdown_links ~full
