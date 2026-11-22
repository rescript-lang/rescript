open Lsp

(* URI serialization normalizes drive letters, unlike Uri.equal/hash. Keep
   the original client URI in the document, but use its wire form as the key. *)
type t = {documents: (string, Document.t) Hashtbl.t}

let make () = {documents = Hashtbl.create 50}

let raise ~code ~message =
  Jsonrpc.Response.Error.raise (Jsonrpc.Response.Error.make ~code ~message ())

let add t ~doc =
  let uri = Document.uri doc |> Uri.to_string in
  (match Hashtbl.mem t.documents uri with
  | false -> Hashtbl.add t.documents uri doc
  | true ->
    raise ~code:RequestFailed
      ~message:
        (Printf.sprintf "Cannot open document %s: it is already open." uri));
  t

let update t ~doc =
  let uri = Document.uri doc |> Uri.to_string in
  (match Hashtbl.find_opt t.documents uri with
  | None ->
    raise ~code:RequestFailed
      ~message:(Printf.sprintf "Cannot update document %s: it is not open." uri)
  | Some _ -> Hashtbl.replace t.documents uri doc);
  t

let remove t ~uri =
  let uri = Uri.to_string uri in
  (match Hashtbl.mem t.documents uri with
  | true -> Hashtbl.remove t.documents uri
  | false ->
    raise ~code:RequestFailed
      ~message:(Printf.sprintf "Cannot close document %s: it is not open." uri));
  t

let get_opt t ~uri = Hashtbl.find_opt t.documents (Uri.to_string uri)

let get t ~uri =
  match get_opt t ~uri with
  | Some doc -> doc
  | None ->
    raise ~code:RequestFailed
      ~message:
        (Printf.sprintf "Cannot get document %s: it is not open."
           (Uri.to_string uri))

let%expect_test "drive-letter variants identify the same open document" =
  let upper = Uri.of_string "file:///D%3A/project/Broken.res" in
  let lower = Uri.of_string "file:///d%3A/project/Broken.res" in
  let make_doc uri text version =
    Document.make
      (Lsp.Types.DidOpenTextDocumentParams.create
         ~textDocument:
           (Lsp.Types.TextDocumentItem.create ~uri ~text ~version
              ~languageId:(Lsp.Types.LanguageKind.Other "rescript")))
      ~position_encoding:`UTF16
  in
  let store = make () in
  let doc = make_doc upper "let value = 1" 0 in
  ignore (add store ~doc);
  Printf.printf "alternate drive lookup: %b\n" (get store ~uri:lower = doc);
  Printf.printf "original client URI retained: %b\n"
    (Uri.equal (Document.uri (get store ~uri:lower)) upper);
  (try
     ignore (add store ~doc:(make_doc lower "let value = 2" 0));
     failwith "Expected a duplicate-document error"
   with Jsonrpc.Response.Error.E error -> print_endline error.message);
  ignore (update store ~doc:(make_doc lower "let value = 3" 1));
  Printf.printf "updated through alternate drive: %s\n"
    (Document.text (get store ~uri:upper));
  ignore (remove store ~uri:upper);
  Printf.printf "closed through alternate drive: %b\n"
    (get_opt store ~uri:lower = None);
  [%expect
    {|
    alternate drive lookup: true
    original client URI retained: true
    Cannot open document file:///d%3A/project/Broken.res: it is already open.
    updated through alternate drive: let value = 3
    closed through alternate drive: true
    |}]
