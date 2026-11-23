open Lsp
open Lsp.Types

type t = Text_document.t

type kind = Analysis.Files.classified_file

let kind_of_uri uri =
  let p = Uri.to_path uri in
  match p |> Filename.extension with
  | ".res" -> Analysis.Files.Res
  | ".resi" -> Analysis.Files.Resi
  | other ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:("unsupported file extension for " ^ p)
         ~data:(`Assoc [("extension", `String other)])
         ())

let kind doc = doc |> Text_document.documentUri |> kind_of_uri

let make (doc : DidOpenTextDocumentParams.t) ~position_encoding =
  let t_doc = Text_document.make ~position_encoding doc in
  t_doc

let text t = Text_document.text t

let uri t = Text_document.documentUri t

let update_text ~version t changes =
  match Text_document.apply_content_changes ~version t changes with
  | exception Text_document.Invalid_utf _ ->
    (* TODO: add log *)
    t
  | tdoc -> tdoc

let range_of_text (text : string) : Range.t =
  let text_length = String.length text in
  let rec end_position offset line character =
    if offset = text_length then Position.create ~line ~character
    else
      match String.get text offset with
      | '\n' -> end_position (offset + 1) (line + 1) 0
      | _ -> end_position (offset + 1) line (character + 1)
  in
  Range.create
    ~start:(Position.create ~line:0 ~character:0)
    ~end_:(end_position 0 0 0)
