open Lsp
open Types

let formatting doc =
  Client_request.TextDocumentFormatting
    (DocumentFormattingParams.create ~textDocument:(Test.text_document doc)
       ~options:(FormattingOptions.create ~tabSize:2 ~insertSpaces:true ())
       ())

let ignores_invalid_document ~extension ~text ~corrected =
  Test.with_project ~name:"format-invalid" ~run_build:false ~text:{||}
    (fun ~root ~doc:_ client ->
      let doc =
        Test.make_document ~text
          ~uri:(Uri.of_path (Filename.concat root ("Invalid." ^ extension)))
      in
      Test.perform client (Test.open_document doc);
      Test.synchronize client;
      (* Ignore the unbuilt-workspace notification, but retain any messages
         caused by formatting incomplete code. *)
      ignore (Test.Client.take_notifications client);
      let request = formatting doc in
      for _ = 1 to 3 do
        Test.snapshot ~root client request
      done;
      Test.change_document client doc ~version:1 ~text:corrected;
      Test.snapshot ~root client request;
      Test.synchronize client;
      let messages =
        Test.Client.take_notifications client
        |> List.filter (fun (n : Jsonrpc.Notification.t) ->
            n.method_ = "window/showMessage")
      in
      Printf.printf "formatting messages: %d\n" (List.length messages))

let%expect_test "ignores repeated formatting of an invalid implementation" =
  ignores_invalid_document ~extension:"res" ~text:{|let value =|}
    ~corrected:{|let value=1|};
  [%expect
    {|
    null
    null
    null
    [
      {
        "newText": "let value = 1\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    formatting messages: 0
    |}]

let%expect_test "ignores repeated formatting of an invalid interface" =
  ignores_invalid_document ~extension:"resi" ~text:{|let value:|}
    ~corrected:{|let value:int|};
  [%expect
    {|
    null
    null
    null
    [
      {
        "newText": "let value: int\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    formatting messages: 0
    |}]

let%expect_test "formats an unsaved buffer without a workspace-local CLI" =
  Test.with_project ~name:"format-buffer" ~run_build:false
    ~text:{|let saved = 0
|} (fun ~root ~doc client ->
      Printf.printf "workspace-local CLI: %b\n"
        (Sys.file_exists (Filename.concat root "node_modules/.bin/rescript"));
      Test.change_document client doc ~version:1 ~text:{|let unsaved=1|};
      Test.snapshot ~root client (formatting doc));
  [%expect
    {|
    workspace-local CLI: false
    [
      {
        "newText": "let unsaved = 1\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "returns no edits for an already formatted buffer" =
  Test.with_project ~name:"format-unchanged" ~run_build:false
    ~text:{|let value = 1
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (formatting doc));
  [%expect {| [] |}]
