open Lsp
open Types

let definition doc pos =
  Client_request.TextDocumentDefinition
    (DefinitionParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

let%expect_test "finds a local value declaration" =
  Test.with_project ~name:"definition" ~text:{|let xx = 10
let y = xx
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (1, 8)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 6, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds a type inside a nested module" =
  Test.with_project ~name:"definition"
    ~text:
      {|module Inner = {
  type tInner = int
}
type typeInner = Inner.tInner
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (3, 24)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 13, "line": 1 },
          "start": { "character": 7, "line": 1 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds a value declaration in an open interface" =
  Test.with_project ~name:"definition" ~text:{|let y = 4
|}
    ~files:[("Test.resi", {|let y: int
|})]
    (fun ~root ~doc:_ client ->
      let doc =
        Test.make_document
          ~uri:(Uri.of_path (Filename.concat root "Test.resi"))
          ~text:{|let y: int
|}
      in
      Test.perform client (Test.open_document doc);
      Test.snapshot ~root client (definition doc (0, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Test.resi"
      }
    ]
    |}]

let%expect_test "finds an exported value in another file" =
  Test.with_project ~name:"definition" ~text:{|let y = Other.x
|}
    ~files:[("Other.res", {|let x = 12
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (0, 14)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Other.res"
      }
    ]
    |}]

let%expect_test "returns no definition in a comment" =
  Test.with_project ~name:"definition" ~text:{|let x = 12
// x
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (1, 3)));
  [%expect {| null |}]
