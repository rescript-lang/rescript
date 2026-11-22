open Lsp
open Types

let with_project f =
  Test.with_project ~name:"protocol-errors" ~run_build:false
    ~text:{|let original = 1|} f

(* A successful request after each rejected message checks both liveness and
   preservation of the initialized state and the original document contents. *)
let check_document client doc =
  ignore
    (Test.Client.request client
       (Client_request.TextDocumentDiagnostic
          (DocumentDiagnosticParams.create
             ~textDocument:(Test.text_document doc) ())));
  match
    Test.Client.request client
      (Client_request.DocumentSymbol
         (DocumentSymbolParams.create ~textDocument:(Test.text_document doc) ()))
  with
  | Some (`DocumentSymbol symbols) ->
    List.iter
      (fun (symbol : DocumentSymbol.t) -> print_endline symbol.name)
      symbols
  | _ -> failwith "Expected document symbols"

let request_error client request =
  let id = Test.Client.send_request client request in
  match (Test.Client.read_response client id).result with
  | Error error -> error
  | Ok _ -> failwith "Expected an error response"

let print_request_error client request =
  let error = request_error client request in
  print_endline (Jsonrpc.Response.Error.Code.to_string error.code)

let%expect_test "preserves request exception codes, messages, and data" =
  with_project (fun ~root ~doc client ->
      let uri = Uri.of_path (Filename.concat root "Unsupported.txt") in
      let error =
        request_error client
          (Client_request.ExecuteCommand
             (ExecuteCommandParams.create ~command:"rescript/createInterface"
                ~arguments:
                  [
                    `String (Uri.to_string uri);
                    `String
                      (Uri.to_string
                         (Uri.of_path (Filename.concat root "Unsupported.cmi")));
                  ]
                ()))
      in
      assert (
        error.message = "unsupported file extension for " ^ Uri.to_path uri);
      print_endline (Jsonrpc.Response.Error.Code.to_string error.code);
      Option.get error.data |> Yojson.Safe.pretty_to_string |> print_endline;
      check_document client doc);
  [%expect
    {|
    InvalidRequest
    { "extension": ".txt" }
    original
    |}]

let%expect_test "converts unexpected request exceptions to internal errors" =
  with_project (fun ~root:_ ~doc client ->
      (* The custom handler decodes its own parameters. A missing URI raises
         inside the handler rather than during initial request decoding. *)
      let error =
        request_error client
          (Client_request.UnknownRequest
             {meth = "textDocument/openCompiled"; params = Some (`Assoc [])})
      in
      Jsonrpc.Response.Error.yojson_of_t error
      |> Yojson.Safe.pretty_to_string |> print_endline;
      check_document client doc);
  [%expect
    {|
    {
      "data": "Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(_, _)",
      "code": -32603,
      "message": "uncaught exception"
    }
    original
    |}]

let print_notification_errors client =
  Test.Client.stderr client |> String.split_on_char '\n'
  |> List.iter (fun line ->
      if
        String.starts_with ~prefix:"Uncaught error when handling notification"
          line
      then print_endline "Rejected notification")

let%expect_test "rejects hover on an unopened file without ending the session" =
  with_project (fun ~root ~doc client ->
      let uri = Uri.of_path (Filename.concat root "Unopened.res") in
      print_request_error client
        (Client_request.TextDocumentHover
           (HoverParams.create
              ~textDocument:(TextDocumentIdentifier.create ~uri)
              ~position:(Position.create ~line:0 ~character:0)
              ()));
      check_document client doc);
  [%expect {|
    RequestFailed
    original
    |}]

let%expect_test
    "rejects completion on an unopened file without ending the session" =
  with_project (fun ~root ~doc client ->
      let uri = Uri.of_path (Filename.concat root "Unopened.res") in
      print_request_error client
        (Client_request.TextDocumentCompletion
           (CompletionParams.create
              ~textDocument:(TextDocumentIdentifier.create ~uri)
              ~position:(Position.create ~line:0 ~character:0)
              ()));
      check_document client doc);
  [%expect {|
    RequestFailed
    original
    |}]

let%expect_test "duplicate didOpen preserves the original document" =
  with_project (fun ~root:_ ~doc client ->
      let replacement = {doc with text = "let replacement = 2"} in
      Test.perform client (Test.open_document replacement);
      check_document client doc;
      print_notification_errors client);
  [%expect {|
    original
    Rejected notification
    |}]

let close_document client uri =
  Test.Client.send_notification client
    (Client_notification.TextDocumentDidClose
       (DidCloseTextDocumentParams.create
          ~textDocument:(TextDocumentIdentifier.create ~uri)))

let%expect_test "rejects didClose for an unopened file and continues" =
  with_project (fun ~root ~doc client ->
      close_document client (Uri.of_path (Filename.concat root "Unopened.res"));
      check_document client doc;
      print_notification_errors client);
  [%expect {|
    original
    Rejected notification
    |}]

let reject_open client uri =
  Test.perform client
    (Test.open_document (Test.make_document ~uri ~text:"let unsupported = 1"));
  (* A rejected open must not have inserted anything into the store. *)
  close_document client uri

let%expect_test "rejects untitled buffers without storing them" =
  with_project (fun ~root:_ ~doc client ->
      reject_open client (Uri.of_string "untitled:Untitled-1");
      check_document client doc;
      print_notification_errors client);
  [%expect {|
    original
    Rejected notification
    |}]

let%expect_test "rejects non-ReScript extensions without storing them" =
  with_project (fun ~root ~doc client ->
      reject_open client (Uri.of_path (Filename.concat root "Unsupported.txt"));
      check_document client doc;
      print_notification_errors client);
  [%expect {|
    original
    Rejected notification
    |}]

let%expect_test "invalid request parameters preserve the initialized state" =
  with_project (fun ~root:_ ~doc client ->
      print_request_error client
        (Client_request.UnknownRequest
           {meth = "textDocument/hover"; params = Some (`Assoc [])});
      check_document client doc);
  [%expect {|
    InternalError
    original
    |}]

let%expect_test "invalid notification parameters do not end the session" =
  with_project (fun ~root:_ ~doc client ->
      Test.Client.send_packet client
        (Jsonrpc.Packet.Notification
           (Jsonrpc.Notification.create ~method_:"textDocument/didOpen"
              ~params:(`Assoc []) ()));
      check_document client doc;
      print_notification_errors client);
  [%expect {|
    original
    Rejected notification
    |}]

let check_body_recovery client doc bodies =
  Test.synchronize client;
  List.iter (Test.Client.send_body client) bodies;
  check_document client doc;
  Test.Client.request client Client_request.Shutdown;
  Test.Client.send_notification client Client_notification.Exit;
  let status = Test.Client.stop client in
  let stderr = Test.Client.stderr client in
  if stderr <> "" then failwith ("Unexpected stderr: " ^ stderr);
  match status with
  | Unix.WEXITED code -> Printf.printf "Exit code: %d\n" code
  | _ -> failwith "Expected a normal exit"

let%expect_test "incomplete JSON is skipped and keeps the session running" =
  with_project (fun ~root:_ ~doc client ->
      check_body_recovery client doc [{|{"jsonrpc":|}]);
  [%expect {|
    original
    Exit code: 0
    |}]

let%expect_test "invalid JSON tokens are skipped and keep the session running" =
  with_project (fun ~root:_ ~doc client ->
      check_body_recovery client doc [{|not JSON|}]);
  [%expect {|
    original
    Exit code: 0
    |}]

let%expect_test
    "consecutive invalid bodies do not prevent reading the next message" =
  with_project (fun ~root:_ ~doc client ->
      check_body_recovery client doc [{|{"jsonrpc":|}; {|not JSON|}]);
  [%expect {|
    original
    Exit code: 0
    |}]

let%expect_test
    "invalid JSON-RPC packets are skipped and keep the session running" =
  with_project (fun ~root:_ ~doc client ->
      check_body_recovery client doc [{|42|}]);
  [%expect {|
    original
    Exit code: 0
    |}]

let%expect_test "null params are rejected without shutting down the server" =
  with_project (fun ~root:_ ~doc client ->
      check_body_recovery client doc
        [{|{"jsonrpc":"2.0","id":100,"method":"shutdown","params":null}|}]);
  [%expect {|
    original
    Exit code: 0
    |}]

let%expect_test "shutdown without params followed by exit succeeds" =
  with_project (fun ~root:_ ~doc:_ client ->
      let id = Test.Client.send_request client Client_request.Shutdown in
      let response = Test.Client.read_response client id in
      assert (response.result = Ok `Null);
      print_endline "Shutdown accepted";
      Test.Client.send_notification client Client_notification.Exit;
      match Test.Client.stop client with
      | Unix.WEXITED code -> Printf.printf "Exit code: %d\n" code
      | _ -> failwith "Expected a normal exit");
  [%expect {|
    Shutdown accepted
    Exit code: 0
    |}]
