open Lsp
open Types

let notify_log ~root ~type_ client =
  Test.Client.send_notification client
    (Client_notification.DidChangeWatchedFiles
       (DidChangeWatchedFilesParams.create
          ~changes:
            [
              FileEvent.create ~type_
                ~uri:(Uri.of_path (Filename.concat root "lib/bs/.compiler.log"));
            ]))

let test_recovery type_ =
  Test.with_project ~name:"first-build" ~run_build:false
    ~text:{|let value = 1
|} (fun ~root ~doc client ->
      let hover =
        Client_request.TextDocumentHover
          (HoverParams.create ~textDocument:(Test.text_document doc)
             ~position:(Test.position (0, 5))
             ())
      in
      Test.snapshot ~root client hover;
      ignore (Test.Client.take_notifications client);
      (* An early log event can arrive before build metadata exists. It must
         leave the session usable and must not repeat initialization errors. *)
      notify_log ~root ~type_ client;
      Test.synchronize client;
      let messages =
        Test.Client.take_notifications client
        |> List.filter (fun (n : Jsonrpc.Notification.t) ->
            n.method_ = "window/showMessage")
      in
      Printf.printf "repeated initialization messages: %d\n"
        (List.length messages);
      Test.run_rescript_build ~root;
      notify_log ~root ~type_ client;
      Test.snapshot ~root client hover;
      (* This request uses compiler_config, not only the analysis package cache. *)
      Test.snapshot ~root client
        (Client_request.UnknownRequest
           {
             meth = "textDocument/openCompiled";
             params =
               Some
                 (Jsonrpc.Structured.t_of_yojson
                    (TextDocumentIdentifier.yojson_of_t (Test.text_document doc)));
           });
      Test.change_document client doc ~version:1 ~text:{|let unsaved = 2
|};
      notify_log ~root ~type_:FileChangeType.Changed client;
      let symbols =
        Client_request.DocumentSymbol
          (DocumentSymbolParams.create ~textDocument:(Test.text_document doc) ())
      in
      (* The unsaved buffer survives rediscovery; disk still has a valid value. *)
      Test.snapshot ~root client symbols)

let%expect_test "recovers after the first compiler log is created" =
  test_recovery FileChangeType.Created;
  [%expect
    {|
    null
    repeated initialization messages: 0
    { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } }
    { "uri": "file:///workspace/lib/es6/./Test.res.js" }
    [
      {
        "kind": 16,
        "name": "unsaved",
        "range": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "selectionRange": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "recovers when the client reports the first build as a change" =
  test_recovery FileChangeType.Changed;
  [%expect
    {|
    null
    repeated initialization messages: 0
    { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } }
    { "uri": "file:///workspace/lib/es6/./Test.res.js" }
    [
      {
        "kind": 16,
        "name": "unsaved",
        "range": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "selectionRange": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]
