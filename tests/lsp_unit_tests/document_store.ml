open Lsp
open Rescript_language_server_core
open Rescript_language_server_core.Document_store

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
