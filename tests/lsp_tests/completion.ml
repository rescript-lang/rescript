open Lsp
open Types

let completion doc pos =
  Client_request.TextDocumentCompletion
    (CompletionParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

let%expect_test "completes a string function after a pipe" =
  let text = {|let value = "abc"
|} in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1 ~text:(text ^ {|value->toU|});
      Test.snapshot ~root client (completion doc (1, 10)));
  [%expect
    {|
    [
      {
        "detail": "string => string",
        "documentation": {
          "kind": "markdown",
          "value": "\n`toUpperCase(str)` converts `str` to upper case using the locale-insensitive\ncase mappings in the Unicode Character Database. Notice that the conversion can\nexpand the number of letters in the result, for example the German ß\ncapitalizes to two Ses in a row.\nSee [`String.toUpperCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase) on MDN.\n\n## Examples\n\n```rescript\nString.toUpperCase(\"abc\") == \"ABC\"\nString.toUpperCase(`Straße`) == `STRASSE`\nString.toUpperCase(`πς`) == `ΠΣ`\n```\n"
        },
        "kind": 12,
        "label": "String.toUpperCase",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes a named argument in a function call" =
  let text =
    {|module Lib = {
  let foo = (~age, ~name) => name ++ Int.toString(age)
}
|}
  in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let x = Lib.foo(~na|});
      Test.snapshot ~root client (completion doc (3, 19)));
  [%expect
    {| [ { "detail": "string", "kind": 4, "label": "name", "tags": [] } ] |}]

let%expect_test "completes a constructor from the expected variant type" =
  let text = {|type someVariant = One | Two | Three(int, string)
|} in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let x: someVariant = O|});
      Test.snapshot ~root client (completion doc (1, 22)));
  [%expect
    {|
    [
      {
        "detail": "One",
        "documentation": {
          "kind": "markdown",
          "value": "```rescript\nOne\n```\n\n```rescript\ntype someVariant = One | Two | Three(int, string)\n```"
        },
        "insertText": "One",
        "insertTextFormat": 2,
        "kind": 4,
        "label": "One",
        "sortText": "A One",
        "tags": []
      },
      { "detail": "module Option", "kind": 9, "label": "Option", "tags": [] },
      { "detail": "module Ordering", "kind": 9, "label": "Ordering", "tags": [] },
      { "detail": "module Object", "kind": 9, "label": "Object", "tags": [] },
      {
        "data": { "modulePath": "Obj", "filePath": "/workspace/Test.res" },
        "detail": "module Obj",
        "kind": 9,
        "label": "Obj",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes a module member in a dictionary literal with spread"
    =
  let text =
    {|module Colors = {
  let red = "red"
  let green = "green"
}
let base = dict{"a": Colors.red}
|}
  in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let x = dict{...base, "b": Colors.g}|});
      Test.snapshot ~root client (completion doc (5, 33)));
  [%expect
    {| [ { "detail": "string", "kind": 12, "label": "green", "tags": [] } ] |}]

let%expect_test "completes a record field in a destructuring pattern" =
  let text =
    {|type someRecord = {first: int, second: bool}
let f = {first: 123, second: true}
|}
  in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1 ~text:(text ^ {|let {fi} = f|});
      Test.snapshot ~root client (completion doc (2, 7)));
  [%expect
    {|
    [
      {
        "detail": "int",
        "documentation": {
          "kind": "markdown",
          "value": "```rescript\nfirst: int\n```\n\n```rescript\ntype someRecord = {first: int, second: bool}\n```"
        },
        "kind": 5,
        "label": "first",
        "tags": []
      }
    ]
    |}]

let%expect_test
    "completes a documented deprecated module member in an unsaved buffer" =
  let text =
    {|module Dep: {
  @res.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}
|}
  in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let cc = Dep.c|});
      Test.snapshot ~root client (completion doc (6, 14)));
  [%expect
    {|
    [
      {
        "deprecated": true,
        "detail": "int => int",
        "documentation": {
          "kind": "markdown",
          "value": "Deprecated: Use customDouble instead\n\nSome doc comment"
        },
        "kind": 12,
        "label": "customDouble",
        "tags": [ 1 ]
      }
    ]
    |}]

let%expect_test "completes fields on a record receiver" =
  let text =
    {|type person = {name: string, age: int}
let person = {name: "Alice", age: 42}
|}
  in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let name = person.na|});
      Test.snapshot ~root client (completion doc (2, 20)));
  [%expect
    {|
    [
      {
        "detail": "string",
        "documentation": {
          "kind": "markdown",
          "value": "```rescript\nname: string\n```\n\n```rescript\ntype person = {name: string, age: int}\n```"
        },
        "kind": 5,
        "label": "name",
        "tags": []
      }
    ]
    |}]

let%expect_test "does not offer a module member with an unmatched prefix" =
  let text =
    {|module Lib = {
  let next = (~number=0, ~year) => number + year
}
|}
  in
  Test.with_project ~name:"completion" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let x = Lib.zzzz|});
      Test.snapshot ~root client (completion doc (3, 16)));
  [%expect {| [] |}]
