open Lsp
open Types

let capabilities =
  {
    Test.client_capabilities with
    window =
      Some
        (WindowClientCapabilities.create
           ~showDocument:(ShowDocumentClientCapabilities.create ~support:true)
           ());
  }

let execute ?arguments command =
  Client_request.ExecuteCommand
    (ExecuteCommandParams.create ~command ?arguments ())

let read_dump client command inspect =
  let id = Test.Client.send_request client command in
  let request = Test.Client.read_request client "window/showDocument" in
  let params =
    Option.get request.params |> Jsonrpc.Structured.yojson_of_t
    |> ShowDocumentParams.t_of_yojson
  in
  let path = Uri.to_path params.uri in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Printf.printf "temporary file: %b\n"
        (Unix.realpath (Filename.dirname path)
        = Unix.realpath (Filename.get_temp_dir_name ()));
      Printf.printf "extension: %s\n" (Filename.extension path);
      Printf.printf "takeFocus: %b\n" (params.takeFocus = Some true);
      In_channel.with_open_bin path In_channel.input_all |> inspect;
      Test.Client.send_packet client
        (Jsonrpc.Packet.Response
           (Jsonrpc.Response.ok request.id (`Assoc [("success", `Bool true)])));
      let response = Test.Client.read_response client id in
      match response.result with
      | Ok json -> Yojson.Safe.to_string json |> print_endline
      | Error error -> failwith error.message);
  path

let%expect_test "opens server state as JSON in a unique temporary file" =
  Test.with_project ~name:"dump-state" ~capabilities ~run_build:false
    ~text:{|let value = 1|} (fun ~root:_ ~doc client ->
      let inspect content =
        let open Yojson.Safe.Util in
        let state = Yojson.Safe.from_string content in
        let document =
          state |> member "store" |> member (Uri.to_string doc.uri)
        in
        Printf.printf "contains open document: %b\n" (document <> `Null)
      in
      let first =
        read_dump client (execute "rescript/dumpServerState") inspect
      in
      let second =
        read_dump client (execute "rescript/dumpServerState") inspect
      in
      Printf.printf "unique paths: %b\n" (first <> second));
  [%expect
    {|
    temporary file: true
    extension: .json
    takeFocus: true
    contains open document: true
    null
    temporary file: true
    extension: .json
    takeFocus: true
    contains open document: true
    null
    unique paths: true
    |}]

let%expect_test "opens CMT metadata as text in a temporary file" =
  Test.with_project ~name:"dump-cmt" ~capabilities ~text:{|let value = 1|}
    (fun ~root:_ ~doc client ->
      read_dump client
        (execute
           ~arguments:[`String (Uri.to_string doc.uri)]
           "rescript/dumpCmt")
        (fun content ->
          Printf.printf "contains CMT metadata: %b\n"
            (String.starts_with ~prefix:"file module_name: Test\n" content))
      |> ignore);
  [%expect
    {|
    temporary file: true
    extension: .txt
    takeFocus: true
    contains CMT metadata: true
    null
    |}]

let%expect_test "dump commands report missing showDocument support" =
  Test.with_project ~name:"dump-unsupported" ~text:{|let value = 1|}
    (fun ~root:_ ~doc client ->
      List.iter
        (fun command ->
          let id = Test.Client.send_request client command in
          match (Test.Client.read_response client id).result with
          | Ok _ -> failwith "Expected unsupported client error"
          | Error error ->
            Printf.printf "showDocument error: %b\n"
              (String.ends_with
                 ~suffix:"The client dont support window/showDocument request"
                 error.message))
        [
          execute "rescript/dumpServerState";
          execute
            ~arguments:[`String (Uri.to_string doc.uri)]
            "rescript/dumpCmt";
        ]);
  [%expect {|
    showDocument error: true
    showDocument error: true
    |}]

let%expect_test "dumpCmt still rejects invalid arguments" =
  Test.with_project ~name:"dump-invalid" ~capabilities ~run_build:false
    ~text:{|let value = 1|} (fun ~root:_ ~doc:_ client ->
      let id = Test.Client.send_request client (execute "rescript/dumpCmt") in
      match (Test.Client.read_response client id).result with
      | Ok _ -> failwith "Expected invalid arguments error"
      | Error error -> print_endline error.message);
  [%expect
    {|
    Invalid arguments for workspace/executeCommand rescript/dumpCmt. Expected a list of string: [uri]
    |}]
