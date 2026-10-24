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
