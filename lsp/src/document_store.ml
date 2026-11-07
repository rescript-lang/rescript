open Lsp
type document = {text: string; version: int}

type t = {documents: (Uri.t, Document.t) Hashtbl.t}

let make () = {documents = Hashtbl.create 50}

let raise ~message =
  Jsonrpc.Response.Error.raise
    (Jsonrpc.Response.Error.make ~code:InvalidRequest ~message ())

let add t ~doc =
  let uri = Document.uri doc in
  (match Hashtbl.mem t.documents uri with
  | false -> Hashtbl.add t.documents uri doc
  | true ->
    raise
      ~message:
        (Printf.sprintf "Document store already has %s to open"
           (Uri.to_string uri)));
  t

let update t ~doc =
  let uri = Document.uri doc in
  (match Hashtbl.find_opt t.documents uri with
  | None ->
    raise
      ~message:
        (Printf.sprintf "Document store not found %s to update"
           (Uri.to_string uri))
  | Some _ -> Hashtbl.replace t.documents uri doc);
  t

let remove t ~uri =
  (match Hashtbl.mem t.documents uri with
  | true -> Hashtbl.remove t.documents uri
  | false ->
    raise
      ~message:
        (Printf.sprintf "Document store not found %s to remove"
           (Uri.to_string uri)));
  t

let get_opt t ~uri = Hashtbl.find_opt t.documents uri

let get t ~uri =
  match get_opt t ~uri with
  | Some doc -> doc
  | None ->
    raise
      ~message:
        (Printf.sprintf "Document store not found %s to get" (Uri.to_string uri))
