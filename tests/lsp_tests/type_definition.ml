open Lsp
open Types

let type_definition doc pos =
  Client_request.TextDocumentTypeDefinition
    (TypeDefinitionParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

let%expect_test "finds the variant type of an inferred binding" =
  Test.with_project ~name:"type-definition"
    ~text:{|type variant = Foo | Bar
let x = Foo
|} (fun ~root ~doc client ->
      Test.snapshot ~root client (type_definition doc (1, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 24, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds the record type of an inferred binding" =
  Test.with_project ~name:"type-definition"
    ~text:{|type record = {item: string}
let y = {item: "foo"}
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (type_definition doc (1, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 28, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds a type declared in another file" =
  Test.with_project ~name:"type-definition" ~text:{|let x = Other.Foo
|}
    ~files:[("Other.res", {|type variant = Foo | Bar
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (type_definition doc (0, 4)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 24, "line": 0 },
          "start": { "character": 0, "line": 0 }
        },
        "uri": "file:///workspace/Other.res"
      }
    ]
    |}]
