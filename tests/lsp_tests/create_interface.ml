open Lsp
open Types

let create_interface doc =
  Client_request.UnknownRequest
    {
      meth = "textDocument/createInterface";
      params =
        Some
          (Jsonrpc.Structured.t_of_yojson
             (TextDocumentIdentifier.yojson_of_t (Test.text_document doc)));
    }

let create_and_print ~root ~doc client =
  let response = Test.Client.request client (create_interface doc) in
  Test.print_json ~root response;
  let interface = TextDocumentIdentifier.t_of_yojson response in
  In_channel.with_open_bin (Uri.to_path interface.uri) In_channel.input_all
  |> print_string

let interface_actions client doc =
  Test.Client.request client
    (Client_request.CodeAction
       (CodeActionParams.create ~textDocument:(Test.text_document doc)
          ~range:(Test.range (0, 0) (0, 0))
          ~context:(CodeActionContext.create ~diagnostics:[] ())
          ()))
  |> Option.value ~default:[]
  |> List.filter_map (function
    | `CodeAction (action : CodeAction.t)
      when action.title = "Create interface file" ->
      Some action
    | _ -> None)

let print_interface_actions ~root client doc =
  interface_actions client doc |> List.map CodeAction.yojson_of_t
  |> fun actions -> Test.print_json ~root (`List actions)

let%expect_test "advertises interface creation as an executable command" =
  Test.with_project ~name:"interface-command-capability" ~run_build:false
    ~text:{|let value = 42|} (fun ~root ~doc:_ _ ->
      let client = Test.Client.start () in
      Fun.protect
        ~finally:(fun () -> ignore (Test.Client.stop client))
        (fun () ->
          let initialized =
            Test.Client.request client
              (Client_request.Initialize
                 (InitializeParams.create ~capabilities:Test.client_capabilities
                    ~rootUri:(Uri.of_path root) ()))
          in
          match initialized.capabilities.executeCommandProvider with
          | Some {commands} ->
            List.filter
              (String.starts_with ~prefix:"rescript/createInterface:")
              commands
            |> List.iter (fun command ->
                Test.print_json ~root (`String command))
          | None -> failwith "Expected executable commands"));
  [%expect {| "rescript/createInterface:<instance>" |}]

let%expect_test
    "offers and executes interface creation for a saved implementation" =
  Test.with_project ~name:"interface-code-action"
    ~capabilities:Execute_commands.capabilities ~text:{|let value = 42|}
    (fun ~root ~doc client ->
      assert (Sys.file_exists (Uri.to_path doc.uri));
      print_interface_actions ~root client doc;
      let command =
        match interface_actions client doc with
        | [{command = Some command}] -> command
        | _ -> failwith "Expected one create-interface command"
      in
      assert (List.mem command.command (Test.Client.commands client));
      let id =
        Test.Client.send_request client
          (Client_request.ExecuteCommand
             (ExecuteCommandParams.create ~command:command.command
                ?arguments:command.arguments ()))
      in
      let show_document =
        Test.Client.read_request client "window/showDocument"
      in
      let params =
        Option.get show_document.params
        |> Jsonrpc.Structured.yojson_of_t |> ShowDocumentParams.t_of_yojson
      in
      Test.print_json ~root
        (TextDocumentIdentifier.yojson_of_t
           (TextDocumentIdentifier.create ~uri:params.uri));
      assert (params.takeFocus = Some true);
      Test.Client.send_packet client
        (Jsonrpc.Packet.Response
           (Jsonrpc.Response.ok show_document.id
              (`Assoc [("success", `Bool true)])));
      assert ((Test.Client.read_response client id).result = Ok `Null);
      In_channel.with_open_bin (Uri.to_path params.uri) In_channel.input_all
      |> print_string;
      print_interface_actions ~root client doc);
  [%expect
    {|
    [
      {
        "command": {
          "arguments": [
            "file:///workspace/Test.res",
            "file:///workspace/lib/rescript/Test.cmi"
          ],
          "command": "rescript/createInterface:<instance>",
          "title": "Create interface file"
        },
        "title": "Create interface file"
      }
    ]
    { "uri": "file:///workspace/Test.resi" }
    let value: int
    []
    |}]

let%expect_test "does not offer interface creation without build metadata" =
  Test.with_project ~name:"interface-unbuilt" ~run_build:false
    ~text:{|let value = 42|} (fun ~root ~doc client ->
      print_interface_actions ~root client doc);
  [%expect {| [] |}]

let%expect_test
    "refuses to overwrite a handwritten interface through the custom request" =
  let handwritten = {|// Keep this handwritten interface.
let value: int
|} in
  Test.with_project ~name:"interface-existing" ~text:{|let value = 42|}
    ~files:[("Test.resi", handwritten)]
    (fun ~root ~doc client ->
      print_interface_actions ~root client doc;
      let path = Uri.to_path doc.uri ^ "i" in
      let id = Test.Client.send_request client (create_interface doc) in
      (match (Test.Client.read_response client id).result with
      | Ok _ -> failwith "Expected refusal to overwrite the interface"
      | Error error ->
        assert (
          error.message
          = "Cannot create interface file " ^ path ^ ": it already exists.");
        print_endline "Refused to overwrite Test.resi");
      assert (In_channel.with_open_bin path In_channel.input_all = handwritten);
      print_endline "Handwritten interface unchanged");
  [%expect
    {|
    []
    Refused to overwrite Test.resi
    Handwritten interface unchanged
    |}]

let%expect_test
    "refuses a stale interface-creation command after an interface appears" =
  Test.with_project ~name:"interface-stale-action" ~text:{|let value = 42|}
    (fun ~root:_ ~doc client ->
      let command =
        match interface_actions client doc with
        | [{command = Some command}] -> command
        | _ -> failwith "Expected one create-interface command"
      in
      let path = Uri.to_path doc.uri ^ "i" in
      let handwritten =
        {|// Added after requesting the code action.
let value: int
|}
      in
      Test.write_file path handwritten;
      let id =
        Test.Client.send_request client
          (Client_request.ExecuteCommand
             (ExecuteCommandParams.create ~command:command.command
                ?arguments:command.arguments ()))
      in
      (match (Test.Client.read_response client id).result with
      | Ok _ -> failwith "Expected refusal to overwrite the interface"
      | Error error ->
        assert (
          String.ends_with
            ~suffix:
              ("Cannot create interface file " ^ path ^ ": it already exists.")
            error.message);
        print_endline "Refused stale create-interface command");
      assert (In_channel.with_open_bin path In_channel.input_all = handwritten);
      print_endline "Handwritten interface unchanged");
  [%expect
    {|
    Refused stale create-interface command
    Handwritten interface unchanged
    |}]

let%expect_test "creates an interface for components with optional props" =
  Test.with_project ~name:"create-interface-jsx" ~jsx:true
    ~text:
      {|@react.component
let make = (~name, ~count=3) => React.string(name ++ Int.toString(count))
module Other = {
  @react.component
  let make = (~children) => children
}
|}
    create_and_print;
  [%expect
    {|
    { "uri": "file:///workspace/Test.resi" }
    @react.component
    let make: (~name: string, ~count: int=?) => React.element
    module Other: {
      @react.component
      let make: (~children: React.element) => React.element
    }
    |}]

let%expect_test "preserves functors and module type constraints" =
  Test.with_project ~name:"create-interface"
    ~text:
      {|module type Input = {
  type t
  let value: t
}
module Make = (X: Input) => {
  type t = X.t
  let value = X.value
}
|}
    create_and_print;
  [%expect
    {|
    { "uri": "file:///workspace/Test.resi" }
    module type Input = {
      type t
      let value: t
    }
    module Make: (
      X: Input,
    ) =>
    {
      type t = X.t
      let value: X.t
    }
    |}]

let%expect_test
    "preserves polymorphic variants and external argument attributes" =
  Test.with_project ~name:"create-interface"
    ~text:
      {|type red = [#Ruby | #Redwood | #Rust]
type blue = [#Sapphire | #Neon | #Navy]
type color = [red | blue | #Papayawhip]
external upperBound: ([< #d | #e | #f]) => unit = "myexternal"
external x: (@as("bar") ~foo: int) => unit = "myexternal"
|}
    create_and_print;
  [%expect
    {|
    { "uri": "file:///workspace/Test.resi" }
    type red = [#Ruby | #Redwood | #Rust]
    type blue = [#Sapphire | #Neon | #Navy]
    type color = [red | blue | #Papayawhip]
    external upperBound: ([< #d | #e | #f]) => unit = "myexternal"
    external x: (@as("bar") ~foo: int) => unit = "myexternal"
    |}]

let%expect_test "creates an interface for records and labeled functions" =
  Test.with_project ~name:"create-interface"
    ~text:{|type r = {name: string, age: int}
let add = (~x, ~y) => x + y
|}
    create_and_print;
  [%expect
    {|
    { "uri": "file:///workspace/Test.resi" }
    type r = {name: string, age: int}
    let add: (~x: int, ~y: int) => int
    |}]

let%expect_test "preserves external declarations and inline constants" =
  Test.with_project ~name:"create-interface"
    ~text:
      {|@module("path") external dirname: string => string = "dirname"
@inline let f1 = 10
|}
    create_and_print;
  [%expect
    {|
    { "uri": "file:///workspace/Test.resi" }
    @module("path") external dirname: string => string = "dirname"
    @inline let f1: int
    |}]
