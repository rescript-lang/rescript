open Lsp
open Types

let completion doc pos =
  Client_request.TextDocumentCompletion
    (CompletionParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

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
