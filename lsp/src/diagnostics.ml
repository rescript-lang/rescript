module UriMap = Map.Make (Lsp.Uri)

type t = Lsp.Types.Diagnostic.t list UriMap.t

let create () = UriMap.empty
