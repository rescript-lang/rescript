open Lsp
open Lsp.Types

let ( // ) = Filename.concat

let print_hover_response resp =
  match resp with
  | Some hover ->
    Hover.yojson_of_t hover |> Yojson.Safe.pretty_to_string |> print_endline
  | None -> print_endline "None"

let hover ~uri ~position =
  Test.Request
    ( Client_request.TextDocumentHover
        {
          HoverParams.position;
          textDocument = TextDocumentIdentifier.create ~uri;
          workDoneToken = None;
        },
      print_hover_response )

let test_root name =
  Filename.get_temp_dir_name ()
  // Printf.sprintf "rescript-lsp-%s-%d" name (Unix.getpid ())

let rescript_json =
  {|
{
  "name": "lsp-hover-test",
  "sources": {
    "dir": "."
  },
  "package-specs": {
    "module": "esmodule",
    "in-source": false
  },
  "suffix": ".res.js"
}
|}

let%expect_test "returns type inferred under cursor" =
  let root = test_root "hover" in
  let uri = Uri.of_path (root // "Hover.res") in

  let text = "let a = 1\n" in
  let doc = Test.make_document ~text ~uri in

  Test.setup_project ~root ~rescript_json ~run_build:true ~documents:[doc];

  Test.with_client ~root (fun client ->
      Test.perform_all client
        [
          Test.open_document doc;
          hover ~uri ~position:(Position.create ~line:0 ~character:4);
        ]);

  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]
