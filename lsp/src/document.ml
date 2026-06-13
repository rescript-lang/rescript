type kind = Analysis.Files.classified_file
let kind uri =
  match Filename.extension (Lsp.Types.DocumentUri.to_string uri) with
  | ".res" -> Analysis.Files.Res
  | ".resi" -> Analysis.Files.Resi
  | other ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:"unsupported file extension"
         ~data:(`Assoc [("extension", `String other)])
         ())

let to_string (kind : kind) =
  match kind with
  | Res -> "res"
  | Resi -> "resi"
  | Other -> assert false
