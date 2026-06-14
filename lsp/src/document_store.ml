type document = {text: string; version: int}

type t = {documents: (Lsp.Uri.t, document) Hashtbl.t}

let create () = {documents = Hashtbl.create 25}

let raise ~message =
  Jsonrpc.Response.Error.raise
    (Jsonrpc.Response.Error.make ~code:InternalError ~message ())

let add t ~uri ~text ~version =
  (match Hashtbl.mem t.documents uri with
  | false -> Hashtbl.add t.documents uri {text; version}
  | true ->
    raise
      ~message:
        (Printf.sprintf "Document store already has %s to open"
           (Lsp.Uri.to_string uri)));
  t

let update t ~uri ~text ~version =
  (match Hashtbl.find_opt t.documents uri with
  | None ->
    raise
      ~message:
        (Printf.sprintf "Document store not found %s to update"
           (Lsp.Uri.to_string uri))
  | Some _ -> Hashtbl.replace t.documents uri {text; version});
  t

let remove t ~uri =
  (match Hashtbl.mem t.documents uri with
  | true -> Hashtbl.remove t.documents uri
  | false ->
    raise
      ~message:
        (Printf.sprintf "Document store not found %s to remove"
           (Lsp.Uri.to_string uri)));
  t

let get_opt t ~uri = Hashtbl.find_opt t.documents uri

let get t ~uri =
  match get_opt t ~uri with
  | Some doc -> doc
  | None ->
    raise
      ~message:
        (Printf.sprintf "Document store not found %s to get"
           (Lsp.Uri.to_string uri))
