open Lsp
open Types

let definition doc pos =
  Client_request.TextDocumentDefinition
    (DefinitionParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

let%expect_test "follows a module alias to a value in another file" =
  Test.with_project ~name:"definition"
    ~text:{|module Alias = Other
let x = Alias.value
|}
    ~files:[("Other.res", {|let value = 42
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (1, 16)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 9, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///workspace/Other.res"
      }
    ]
    |}]

let%expect_test "finds a value bound by tuple destructuring" =
  Test.with_project ~name:"definition"
    ~text:
      {|module A = {
  let makeX = () => (1, 2)
  let (xxx, yyy) = makeX()
}
let y = A.xxx
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (4, 10)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 10, "line": 2 },
          "start": { "character": 7, "line": 2 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds a value bound by record destructuring" =
  Test.with_project ~name:"definition"
    ~text:
      {|module A = {
  type t = {name: string, age: int}
  let makeT = () => {name: "", age: 0}
  let {name, age} = makeT()
}
let n1 = A.name
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (5, 11)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 11, "line": 3 },
          "start": { "character": 7, "line": 3 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

let%expect_test "finds a React component from a JSX tag" =
  Test.with_project ~name:"definition-jsx" ~jsx:true
    ~text:
      {|module Comp = {
  @react.component
  let make = (~name) => React.string(name)
}
let element = <Comp name="test" />
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (definition doc (4, 16)));
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 10, "line": 2 },
          "start": { "character": 6, "line": 2 }
        },
        "uri": "file:///workspace/Test.res"
      }
    ]
    |}]

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
