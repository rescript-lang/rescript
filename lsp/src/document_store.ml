open Lsp

type t = {documents: (Uri.t, Document.t) Hashtbl.t}

let make () = {documents = Hashtbl.create 50}

let raise ~code ~message =
  Jsonrpc.Response.Error.raise (Jsonrpc.Response.Error.make ~code ~message ())

let add t ~doc =
  let uri = Document.uri doc in
  (match Hashtbl.mem t.documents uri with
  | false -> Hashtbl.add t.documents uri doc
  | true ->
    raise ~code:RequestFailed
      ~message:
        (Printf.sprintf "Cannot open document %s: it is already open."
           (Uri.to_string uri)));
  t

let update t ~doc =
  let uri = Document.uri doc in
  (match Hashtbl.find_opt t.documents uri with
  | None ->
    raise ~code:RequestFailed
      ~message:
        (Printf.sprintf "Cannot update document %s: it is not open."
           (Uri.to_string uri))
  | Some _ -> Hashtbl.replace t.documents uri doc);
  t

let remove t ~uri =
  (match Hashtbl.mem t.documents uri with
  | true -> Hashtbl.remove t.documents uri
  | false ->
    raise ~code:RequestFailed
      ~message:
        (Printf.sprintf "Cannot close document %s: it is not open."
           (Uri.to_string uri)));
  t

let get_opt t ~uri = Hashtbl.find_opt t.documents uri

let get t ~uri =
  match get_opt t ~uri with
  | Some doc -> doc
  | None ->
    raise ~code:RequestFailed
      ~message:
        (Printf.sprintf "Cannot get document %s: it is not open."
           (Uri.to_string uri))
