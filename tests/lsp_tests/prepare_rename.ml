open Lsp
open Types

let prepare_rename doc pos =
  Client_request.TextDocumentPrepareRename
    (PrepareRenameParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

let%expect_test "selects the identifier range for renaming" =
  Test.with_project ~name:"prepare-rename" ~text:{|let x = 12
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (prepare_rename doc (0, 4)));
  [%expect
    {|
    {
      "end": { "character": 5, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    |}]

let%expect_test "selects a labeled parameter use for renaming" =
  Test.with_project ~name:"prepare-rename" ~text:{|let foo = (~xx) => xx + 1
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (prepare_rename doc (0, 19)));
  [%expect
    {|
    {
      "end": { "character": 21, "line": 0 },
      "start": { "character": 19, "line": 0 }
    }
    |}]

let%expect_test "does not prepare a rename inside a comment" =
  Test.with_project ~name:"prepare-rename" ~text:{|let x = 12
// x
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (prepare_rename doc (1, 3)));
  [%expect {| null |}]
