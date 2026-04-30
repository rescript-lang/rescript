module UriMap = Map.Make (Lsp.Uri)

type document = {
  text : string;
  version : int;
}

type t = {
  documents : document UriMap.t;
  diagnostics : Lsp.Types.Diagnostic.t list UriMap.t;
}

let empty = {documents = UriMap.empty; diagnostics = UriMap.empty}

let open_document t ~uri ~text ~version =
  {t with documents = UriMap.add uri {text; version} t.documents}

let update_document t ~uri ~text ~version =
  {t with documents = UriMap.add uri {text; version} t.documents}

let close_document t ~uri =
  {t with documents = UriMap.remove uri t.documents}
