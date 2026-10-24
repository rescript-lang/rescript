module Syntax_test = Document_syntax

open Lsp
open Types

let ( /+ ) = Filename.concat
let push_capabilities = ClientCapabilities.create ()

let notify_log ~root client =
  Test.Client.send_notification client
    (Client_notification.DidChangeWatchedFiles
       (DidChangeWatchedFilesParams.create
          ~changes:
            [
              FileEvent.create ~type_:FileChangeType.Changed
                ~uri:(Uri.of_path (root /+ "lib" /+ "bs" /+ ".compiler.log"));
            ]))

(* Write each build phase explicitly so a partial compiler log is observable
   deterministically, without racing a real compiler watcher. *)
let write_log ~root client content =
  Test.write_file (root /+ "lib" /+ "bs" /+ ".compiler.log") content;
  notify_log ~root client

let publications client =
  Test.synchronize client;
  Test.Client.take_notifications client
  |> List.filter_map (fun (notification : Jsonrpc.Notification.t) ->
      if notification.method_ = "textDocument/publishDiagnostics" then
        Some (Option.get notification.params |> Jsonrpc.Structured.yojson_of_t)
      else None)

let print_publications ~root client =
  publications client |> fun items -> Test.print_json ~root (`List items)

let close_document client doc =
  Test.Client.send_notification client
    (Client_notification.TextDocumentDidClose
       (DidCloseTextDocumentParams.create ~textDocument:(Test.text_document doc)))

let save_document client (doc : TextDocumentItem.t) ~text =
  Test.write_file (Uri.to_path doc.uri) text;
  Test.Client.send_notification client
    (Client_notification.DidSaveTextDocument
       (DidSaveTextDocumentParams.create ~textDocument:(Test.text_document doc)
          ()))

let warning_log =
  {|#Start(1)
Warning number 26
Test.res:1:5-9

unused variable value.

#Done(2)
|}

let syntax_log =
  {|#Start(3)
Syntax error!
Test.res:1:12-12

Did you forget to write an expression here?

#Done(4)
|}

let%expect_test
    "removes a compiler warning after saving and rebuilding clean code" =
  let clean = {|let value = () => 2
|} in
  let warning = {|let value = () => {
  let unused = 1
  2
}
|} in
  Test.with_project ~name:"diagnostics" ~text:clean (fun ~root ~doc client ->
      Test.change_document client doc ~version:1 ~text:warning;
      save_document client doc ~text:warning;
      Test.run_rescript_build ~root;
      notify_log ~root client;
      Test.snapshot ~root client (Syntax_test.diagnostics doc);
      Test.change_document client doc ~version:2 ~text:clean;
      save_document client doc ~text:clean;
      Test.run_rescript_build ~root;
      notify_log ~root client;
      Test.snapshot ~root client (Syntax_test.diagnostics doc));
  [%expect
    {|
    {
      "kind": "full",
      "items": [
        {
          "message": "Warning 26 - unused variable unused.\nFix this by:\n- Deleting the variable if it's not used anymore.\n- Prepending the variable name with `_` (like `_unused`) to ignore that the variable is unused.\n- Using the variable somewhere.",
          "range": {
            "end": { "character": 12, "line": 1 },
            "start": { "character": 6, "line": 1 }
          },
          "severity": 2,
          "source": "ReScript"
        }
      ]
    }
    { "kind": "full", "items": [] }
    |}]

let%expect_test "does not clear pushed warnings during an incomplete build" =
  Test.with_project ~name:"diagnostics" ~capabilities:push_capabilities
    ~text:{|let value = 1
|} (fun ~root ~doc client ->
      ignore (publications client);
      write_log ~root client warning_log;
      print_endline "completed build:";
      print_publications ~root client;
      write_log ~root client {|#Start(3)
|};
      print_endline "build started:";
      print_publications ~root client;
      Test.change_document client doc ~version:1 ~text:doc.text;
      save_document client doc ~text:doc.text;
      print_endline "save during build:";
      print_publications ~root client;
      write_log ~root client warning_log;
      print_endline "next completed build:";
      print_publications ~root client);
  [%expect
    {|
    completed build:
    [
      {
        "diagnostics": [
          {
            "message": "Warning 26 - unused variable value.",
            "range": {
              "end": { "character": 9, "line": 0 },
              "start": { "character": 4, "line": 0 }
            },
            "severity": 2,
            "source": "ReScript"
          }
        ],
        "uri": "file:///workspace/Test.res"
      }
    ]
    build started:
    []
    save during build:
    [
      {
        "diagnostics": [
          {
            "message": "Warning 26 - unused variable value.",
            "range": {
              "end": { "character": 9, "line": 0 },
              "start": { "character": 4, "line": 0 }
            },
            "severity": 2,
            "source": "ReScript"
          }
        ],
        "uri": "file:///workspace/Test.res"
      }
    ]
    next completed build:
    [
      {
        "diagnostics": [
          {
            "message": "Warning 26 - unused variable value.",
            "range": {
              "end": { "character": 9, "line": 0 },
              "start": { "character": 4, "line": 0 }
            },
            "severity": 2,
            "source": "ReScript"
          }
        ],
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test
    "moves compiler diagnostics between pull and push without duplication" =
  Test.with_project ~name:"diagnostics" ~text:{|let value = 1
|}
    (fun ~root ~doc client ->
      ignore (publications client);
      write_log ~root client warning_log;
      print_endline "open document pushes:";
      print_publications ~root client;
      print_endline "open document pull:";
      Test.snapshot ~root client (Syntax_test.diagnostics doc);
      close_document client doc;
      print_endline "closed document pushes:";
      print_publications ~root client;
      Test.perform client (Test.open_document doc);
      print_endline "reopened document pushes:";
      print_publications ~root client;
      print_endline "reopened document pull:";
      Test.snapshot ~root client (Syntax_test.diagnostics doc));
  [%expect
    {|
    open document pushes:
    []
    open document pull:
    {
      "kind": "full",
      "items": [
        {
          "message": "Warning 26 - unused variable value.",
          "range": {
            "end": { "character": 9, "line": 0 },
            "start": { "character": 4, "line": 0 }
          },
          "severity": 2,
          "source": "ReScript"
        }
      ]
    }
    closed document pushes:
    [
      {
        "diagnostics": [
          {
            "message": "Warning 26 - unused variable value.",
            "range": {
              "end": { "character": 9, "line": 0 },
              "start": { "character": 4, "line": 0 }
            },
            "severity": 2,
            "source": "ReScript"
          }
        ],
        "uri": "file:///workspace/Test.res"
      }
    ]
    reopened document pushes:
    [ { "diagnostics": [], "uri": "file:///workspace/Test.res" } ]
    reopened document pull:
    {
      "kind": "full",
      "items": [
        {
          "message": "Warning 26 - unused variable value.",
          "range": {
            "end": { "character": 9, "line": 0 },
            "start": { "character": 4, "line": 0 }
          },
          "severity": 2,
          "source": "ReScript"
        }
      ]
    }
    |}]

let%expect_test "clears unsaved syntax diagnostics when a document closes" =
  Test.with_project ~name:"diagnostics" ~capabilities:push_capabilities
    ~text:{|let value = 1
|} (fun ~root ~doc client ->
      ignore (publications client);
      Test.change_document client doc ~version:1 ~text:{|let value =
|};
      print_endline "unsaved buffer:";
      print_publications ~root client;
      close_document client doc;
      print_endline "closed:";
      print_publications ~root client);
  [%expect
    {|
    unsaved buffer:
    [
      {
        "diagnostics": [
          {
            "message": "This let-binding misses an expression",
            "range": {
              "end": { "character": 0, "line": 1 },
              "start": { "character": 11, "line": 0 }
            },
            "severity": 1,
            "source": "ReScript"
          }
        ],
        "uri": "file:///workspace/Test.res"
      }
    ]
    closed:
    [ { "diagnostics": [], "uri": "file:///workspace/Test.res" } ]
    |}]

let%expect_test
    "keeps saved compiler syntax diagnostics after closing the buffer" =
  Test.with_project ~name:"diagnostics" ~capabilities:push_capabilities
    ~text:{|let value = 1
|} (fun ~root ~doc client ->
      ignore (publications client);
      let text = {|let value =
|} in
      Test.change_document client doc ~version:1 ~text;
      save_document client doc ~text;
      write_log ~root client syntax_log;
      ignore (publications client);
      close_document client doc;
      print_publications ~root client);
  [%expect
    {|
    [
      {
        "diagnostics": [
          {
            "message": "Did you forget to write an expression here?",
            "range": {
              "end": { "character": 12, "line": 0 },
              "start": { "character": 11, "line": 0 }
            },
            "severity": 1,
            "source": "ReScript"
          }
        ],
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test
    "preserves other files type errors while the build has only syntax errors" =
  Test.with_project ~name:"diagnostics" ~text:{|let value = 1
|}
    ~files:[("Other.res", {|let value = 1
|})]
    (fun ~root ~doc client ->
      write_log ~root client
        {|#Start(1)
We've found a bug for you!
Other.res:1:13-13

This has type: int
Somewhere wanted: string

#Done(2)
|};
      Test.synchronize client;
      Test.change_document client doc ~version:1 ~text:{|let value =
|};
      write_log ~root client syntax_log;
      let other =
        Test.make_document ~uri:(Uri.of_path (root /+ "Other.res")) ~text:{||}
      in
      Test.snapshot ~root client (Syntax_test.diagnostics other);
      write_log ~root client {|#Start(5)
#Done(6)
|};
      Test.snapshot ~root client (Syntax_test.diagnostics other));
  [%expect
    {|
    {
      "kind": "full",
      "items": [
        {
          "message": "This has type: int\nSomewhere wanted: string",
          "range": {
            "end": { "character": 13, "line": 0 },
            "start": { "character": 12, "line": 0 }
          },
          "severity": 1,
          "source": "ReScript"
        }
      ]
    }
    { "kind": "full", "items": [] }
    |}]
