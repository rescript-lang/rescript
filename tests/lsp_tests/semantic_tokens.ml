open Lsp
open Types

let tokens doc =
  Client_request.SemanticTokensFull
    (SemanticTokensParams.create ~textDocument:(Test.text_document doc) ())

let%expect_test "encodes semantic tokens for array literals and spreads" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|let xs = [1, 2]
let ys = [...xs, 3]
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (tokens doc));
  [%expect {| { "data": [ 0, 4, 2, 1, 0, 1, 4, 2, 1, 0, 0, 9, 2, 1, 0 ] } |}]

let%expect_test "encodes semantic tokens for list literals and spreads" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|let xs = list{1, 2}
let ys = list{...xs, 3}
|}
    (fun ~root ~doc client -> Test.snapshot ~root client (tokens doc));
  [%expect {| { "data": [ 0, 4, 2, 1, 0, 1, 4, 2, 1, 0, 0, 13, 2, 1, 0 ] } |}]

let%expect_test "clears semantic tokens when the buffer becomes empty" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|let x = 1
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (tokens doc);
      Test.change_document client doc ~version:1 ~text:{||};
      Test.snapshot ~root client (tokens doc));
  [%expect {|
    { "data": [ 0, 4, 1, 1, 0 ] }
    { "data": [] }
    |}]
