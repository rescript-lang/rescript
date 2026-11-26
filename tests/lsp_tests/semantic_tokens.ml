open Lsp
open Types

let tokens doc =
  Client_request.SemanticTokensFull
    (SemanticTokensParams.create ~textDocument:(Test.text_document doc) ())

let%expect_test "encodes tokens inside a dictionary literal" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|let d = dict{"foo": bar}
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (tokens doc));
  [%expect {| { "data": [ 0, 4, 1, 1, 0, 0, 16, 3, 1, 0 ] } |}]

let%expect_test "encodes array access with literal and variable indices" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|let x = xs[0]
let x = xs[index]
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (tokens doc));
  [%expect
    {|
    {
      "data": [
        0, 4, 1, 1, 0, 0, 4, 2, 1, 0, 1, 4, 1, 1, 0, 0, 4, 2, 1, 0, 0, 3, 5, 1, 0
      ]
    }
    |}]

let%expect_test "encodes array mutation" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|xs[0] = 42
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (tokens doc));
  [%expect {| { "data": [ 0, 0, 2, 1, 0 ] } |}]

let%expect_test "encodes nested module types and aliased argument labels" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:
      {|module XX = {
  module YY = {
    type t = int
  }
}
open XX.YY
type tt = t
let add = (~hello as x, ~world) => x + world
let _ = add(~hello=3, ...)
|}
    (fun ~root ~doc client -> Test.snapshot ~root client (tokens doc));
  [%expect
    {|
    {
      "data": [
        0, 7, 2, 4, 0, 1, 9, 2, 4, 0, 1, 9, 1, 2, 0, 0, 4, 3, 2, 0, 3, 5, 2, 4,
        0, 0, 3, 2, 4, 0, 1, 5, 2, 2, 0, 0, 5, 1, 2, 0, 1, 4, 3, 1, 0, 0, 17, 1,
        1, 0, 0, 3, 6, 1, 0, 0, 11, 1, 1, 0, 0, 4, 5, 1, 0, 1, 8, 3, 1, 0
      ]
    }
    |}]

let%expect_test "encodes intrinsic and nested component JSX tags" =
  Test.with_project ~name:"semantic-tokens" ~run_build:false
    ~text:{|let _ = <div scale="abc"> <SomeComponent.Nested /> </div>
|}
    (fun ~root ~doc client -> Test.snapshot ~root client (tokens doc));
  [%expect
    {|
    {
      "data": [
        0, 8, 1, 3, 0, 0, 1, 3, 7, 0, 0, 15, 1, 3, 0, 0, 2, 1, 3, 0, 0, 1, 13, 4,
        0, 0, 14, 6, 4, 0, 0, 7, 1, 3, 0, 0, 3, 1, 3, 0, 0, 2, 3, 7, 0, 0, 3, 1,
        3, 0
      ]
    }
    |}]

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
