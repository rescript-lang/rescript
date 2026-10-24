open Lsp
open Types

let references doc pos =
  Client_request.TextDocumentReferences
    (ReferenceParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos)
       ~context:(ReferenceContext.create ~includeDeclaration:true)
       ())

let%expect_test "finds uses through a module alias in another file" =
  Test.with_project ~name:"references" ~text:{|let x = 12
|}
    ~files:
      [
        ( "Consumer.res",
          {|module Ref = Test
let first = Ref.x
let second = Test.x
|} );
      ]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (references doc (0, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 17, "line": 1 },
          "start": { "character": 16, "line": 1 }
        },
        "uri": "file:///workspace/./Consumer.res"
      },
      {
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 18, "line": 2 }
        },
        "uri": "file:///workspace/./Consumer.res"
      },
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

(* TODO: Generated component bindings currently repeat the declaration range. *)
let%expect_test "finds references to a nested React component from JSX" =
  Test.with_project ~name:"references-jsx" ~jsx:true
    ~text:
      {|module Comp = {
  @react.component
  let make = () => React.null
}
let first = <Comp />
let second = <Comp />
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (references doc (4, 14)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 10, "line": 2 },
          "start": { "character": 6, "line": 2 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 10, "line": 2 },
          "start": { "character": 6, "line": 2 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 10, "line": 2 },
          "start": { "character": 6, "line": 2 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 17, "line": 4 },
          "start": { "character": 13, "line": 4 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 18, "line": 5 },
          "start": { "character": 14, "line": 5 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds the declaration and local uses of a value" =
  Test.with_project ~name:"references"
    ~text:{|let x = 12
let a = x
let b = a
let c = x
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (references doc (0, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 9, "line": 1 },
          "start": { "character": 8, "line": 1 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 9, "line": 3 },
          "start": { "character": 8, "line": 3 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds a labeled parameter and its use" =
  Test.with_project ~name:"references" ~text:{|let foo = (~xx) => xx + 1
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (references doc (0, 19)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 14, "line": 0 },
          "start": { "character": 11, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      },
      {
        "range": {
          "end": { "character": 21, "line": 0 },
          "start": { "character": 19, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds references across implementation interface and consumer"
    =
  Test.with_project ~name:"references" ~text:{|let x = 12
|}
    ~files:
      [("Test.resi", {|let x: int
|}); ("Consumer.res", {|let a = Test.x
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (references doc (0, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 14, "line": 0 },
          "start": { "character": 13, "line": 0 }
        },
        "uri": "file:///workspace/./Consumer.res"
      },
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/./Test.resi"
      },
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]
