(* module UriMap = Map.Make (Lsp.Uri) *)

type document = {text: string; version: int}

type t = {documents: (Lsp.Uri.t, document) Hashtbl.t}

let create () = {documents = Hashtbl.create 25}

let open_document t ~uri ~text ~version =
  Hashtbl.add t.documents uri {text; version};
  t

let update_document t ~uri ~text ~version =
  (match Hashtbl.find_opt t.documents uri with
  | None ->
    raise
      (Failure (Printf.sprintf "Document not found: %s" (Lsp.Uri.to_string uri)))
  | Some _ -> Hashtbl.replace t.documents uri {text; version});
  t

let remove_document t ~uri =
  Hashtbl.remove t.documents uri;
  t

let get_document t ~uri =
  match Hashtbl.find_opt t.documents uri with
  | Some doc -> doc
  | None ->
    raise
      (Failure (Printf.sprintf "Document not found: %s" (Lsp.Uri.to_string uri)))
