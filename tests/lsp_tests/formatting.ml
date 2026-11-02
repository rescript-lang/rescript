open Lsp
open Types

let rejects_invalid_document ~extension ~text ~corrected =
  Test.with_project ~name:"format-invalid" ~run_build:false ~text:{||}
    (fun ~root ~doc:_ client ->
      let executable = Filename.concat root "node_modules/.bin/rescript" in
      let binary =
        In_channel.with_open_bin Test.executable In_channel.input_all
      in
      Test.write_file executable binary;
      Unix.chmod executable 0o755;
      let doc =
        Test.make_document ~text
          ~uri:(Uri.of_path (Filename.concat root ("Invalid." ^ extension)))
      in
      Test.perform client (Test.open_document doc);
      let request =
        Client_request.TextDocumentFormatting
          (DocumentFormattingParams.create
             ~textDocument:(Test.text_document doc)
             ~options:
               (FormattingOptions.create ~tabSize:2 ~insertSpaces:true ())
             ())
      in
      for _ = 1 to 3 do
        let id = Test.Client.send_request client request in
        match (Test.Client.read_response client id).result with
        | Ok _ -> failwith "Expected a formatting error"
        | Error error -> print_endline error.message
      done;
      Test.change_document client doc ~version:1 ~text:corrected;
      Test.snapshot ~root client request)

let%expect_test "rejects repeated formatting of an invalid implementation" =
  rejects_invalid_document ~extension:"res" ~text:{|let value =|}
    ~corrected:{|let value=1|};
  [%expect
    {|
    Failed to format Invalid.res
    Failed to format Invalid.res
    Failed to format Invalid.res
    [
      {
        "newText": "let value = 1\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "rejects repeated formatting of an invalid interface" =
  rejects_invalid_document ~extension:"resi" ~text:{|let value:|}
    ~corrected:{|let value:int|};
  [%expect
    {|
    Failed to format Invalid.resi
    Failed to format Invalid.resi
    Failed to format Invalid.resi
    [
      {
        "newText": "let value: int\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]
