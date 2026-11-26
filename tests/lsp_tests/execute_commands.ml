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

let scoped_command client name =
  Test.Client.commands client
  |> List.find (String.starts_with ~prefix:(name ^ ":"))

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
        read_dump client
          (execute (scoped_command client "rescript/dumpServerState"))
          inspect
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
      List.iter
        (fun command ->
          read_dump client
            (execute ~arguments:[`String (Uri.to_string doc.uri)] command)
            (fun content ->
              Printf.printf "contains CMT metadata: %b\n"
                (String.starts_with ~prefix:"file module_name: Test\n" content))
          |> ignore)
        ["rescript/dumpCmt"; scoped_command client "rescript/dumpCmt"]);
  [%expect
    {|
    temporary file: true
    extension: .txt
    takeFocus: true
    contains CMT metadata: true
    null
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

let%expect_test "advertises disjoint command IDs for simultaneous servers" =
  Test.with_project ~name:"command-instances" ~capabilities ~run_build:false
    ~text:{|let value = 1|} (fun ~root ~doc:_ first ->
      Test.with_client ~capabilities ~root (fun second ->
          let first_commands = Test.Client.commands first in
          let second_commands = Test.Client.commands second in
          Test.print_json ~root
            (`List (List.map (fun name -> `String name) first_commands));
          let suffixes commands =
            List.map
              (fun command ->
                match String.split_on_char ':' command with
                | [_; suffix] -> suffix
                | _ -> failwith "Expected a scoped command")
              commands
            |> List.sort_uniq String.compare
          in
          assert (List.length (suffixes first_commands) = 1);
          assert (List.length (suffixes second_commands) = 1);
          assert (List.length second_commands = List.length first_commands);
          Printf.printf "disjoint command IDs: %b\n"
            (not
               (List.exists
                  (fun command -> List.mem command second_commands)
                  first_commands));
          List.iter
            (fun (client, foreign_command) ->
              let id =
                Test.Client.send_request client (execute foreign_command)
              in
              match (Test.Client.read_response client id).result with
              | Ok _ ->
                failwith "Expected rejection of another server's command"
              | Error error ->
                assert (
                  error.message
                  = "Unknown command " ^ foreign_command
                    ^ " for workspace/executeCommand request");
                print_endline "foreign command rejected")
            [
              (first, scoped_command second "rescript/dumpServerState");
              (second, scoped_command first "rescript/dumpServerState");
            ]));
  [%expect
    {|
    [
      "rescript/openCompiled:<instance>", "rescript/createInterface:<instance>",
      "rescript/switchImplementationInterface:<instance>",
      "rescript/dumpServerState:<instance>", "rescript/dumpCmt:<instance>"
    ]
    disjoint command IDs: true
    foreign command rejected
    foreign command rejected
    |}]

let%expect_test
    "accepts plain and scoped commands without advertising plain IDs" =
  Test.with_project ~name:"command-aliases" ~run_build:false
    ~text:{|let value = 1|} (fun ~root:_ ~doc:_ client ->
      List.iter
        (fun name ->
          assert (not (List.mem name (Test.Client.commands client)));
          List.iter
            (fun command ->
              let id = Test.Client.send_request client (execute command) in
              match (Test.Client.read_response client id).result with
              | Ok _ -> failwith "Expected invalid arguments"
              | Error error ->
                assert (
                  String.starts_with
                    ~prefix:
                      ("Invalid arguments for workspace/executeCommand " ^ name)
                    error.message))
            [name; scoped_command client name];
          Printf.printf "%s: plain and scoped dispatch\n" name)
        [
          "rescript/openCompiled";
          "rescript/createInterface";
          "rescript/switchImplementationInterface";
          "rescript/dumpCmt";
        ]);
  [%expect
    {|
    rescript/openCompiled: plain and scoped dispatch
    rescript/createInterface: plain and scoped dispatch
    rescript/switchImplementationInterface: plain and scoped dispatch
    rescript/dumpCmt: plain and scoped dispatch
    |}]

let%expect_test "navigation code actions use stable advertised IDs" =
  Test.with_project ~name:"command-navigation" ~capabilities
    ~text:{|let value = 1|}
    ~files:[("Test.resi", {|let value: int|})]
    (fun ~root ~doc client ->
      let navigate doc =
        let request =
          Client_request.CodeAction
            (CodeActionParams.create ~textDocument:(Test.text_document doc)
               ~range:(Test.range (0, 0) (0, 0))
               ~context:(CodeActionContext.create ~diagnostics:[] ())
               ())
        in
        let commands () =
          Test.Client.request client request
          |> Option.value ~default:[]
          |> List.filter_map (function
            | `CodeAction (action : CodeAction.t) -> action.command
            | `Command command -> Some command)
        in
        let actions = commands () in
        assert (actions = commands ());
        List.iter
          (fun (command : Command.t) ->
            assert (List.mem command.command (Test.Client.commands client));
            let id =
              Test.Client.send_request client
                (execute ?arguments:command.arguments command.command)
            in
            let request =
              Test.Client.read_request client "window/showDocument"
            in
            let params =
              Option.get request.params |> Jsonrpc.Structured.yojson_of_t
              |> ShowDocumentParams.t_of_yojson
            in
            assert (params.takeFocus = Some true);
            Test.print_json ~root
              (`Assoc
                 [
                   ("command", `String command.command);
                   ("uri", `String (Uri.to_string params.uri));
                 ]);
            Test.Client.send_packet client
              (Jsonrpc.Packet.Response
                 (Jsonrpc.Response.ok request.id
                    (`Assoc [("success", `Bool true)])));
            assert ((Test.Client.read_response client id).result = Ok `Null))
          actions
      in
      navigate doc;
      let interface =
        Test.make_document
          ~uri:(Uri.of_path (Filename.concat root "Test.resi"))
          ~text:{|let value: int|}
      in
      Test.perform client (Test.open_document interface);
      navigate interface);
  [%expect
    {|
    {
      "command": "rescript/openCompiled:<instance>",
      "uri": "file:///workspace/lib/es6/Test.res.js"
    }
    {
      "command": "rescript/switchImplementationInterface:<instance>",
      "uri": "file:///workspace/Test.resi"
    }
    {
      "command": "rescript/openCompiled:<instance>",
      "uri": "file:///workspace/lib/es6/Test.res.js"
    }
    {
      "command": "rescript/switchImplementationInterface:<instance>",
      "uri": "file:///workspace/Test.res"
    }
    |}]
