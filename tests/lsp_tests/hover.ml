open Lsp
open Lsp.Types

let ( /+ ) = Filename.concat

let print_hover_response resp =
  match resp with
  | Some hover ->
    Hover.yojson_of_t hover |> Yojson.Safe.pretty_to_string |> print_endline
  | None -> print_endline "<Empty>"

let hover ~uri ~position =
  Test.Request
    ( Client_request.TextDocumentHover
        {
          HoverParams.position;
          textDocument = TextDocumentIdentifier.create ~uri;
          workDoneToken = None;
        },
      print_hover_response )

let rescript_json =
  {|
{
  "name": "hover-test",
  "sources": {
    "dir": "."
  },
  "package-specs": {
    "module": "esmodule",
    "in-source": false
  },
  "suffix": ".res.js"
}
|}

let test_hover ~text ~position =
  let root = Filename.get_temp_dir_name () /+ "hover" in
  let uri = Uri.of_path (root /+ "test.res") in
  let doc = Test.make_document ~text ~uri in
  Test.setup_project ~root ~rescript_json ~run_build:true ~documents:[doc];
  let line, character = position in
  Test.with_client ~root (fun client ->
      Test.perform_all client
        [
          Test.open_document doc;
          hover ~uri ~position:(Position.create ~line ~character);
        ])
  |> ignore

let%expect_test "shows documentation for an exported value from another file" =
  Test.with_project ~name:"hover" ~text:{|let x = Other.customDouble(2)
|}
    ~files:
      [
        ("Other.res", {|/** Doubles a value. */
let customDouble = x => x * 2
|});
      ]
    (fun ~root ~doc client ->
      Test.snapshot ~root client
        (Client_request.TextDocumentHover
           (HoverParams.create ~textDocument:(Test.text_document doc)
              ~position:(Test.position (0, 18))
              ())));
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint => int\n```\n---\n Doubles a value. "
      }
    }
    |}]

let%expect_test "shows the abstract type exposed by an interface" =
  Test.with_project ~name:"hover" ~text:{|let x = Other.value
|}
    ~files:
      [
        ("Other.res", {|type t = int
let value = 1
|});
        ("Other.resi", {|type t
let value: t
|});
      ]
    (fun ~root ~doc client ->
      Test.snapshot ~root client
        (Client_request.TextDocumentHover
           (HoverParams.create ~textDocument:(Test.text_document doc)
              ~position:(Test.position (0, 15))
              ())));
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nOther.t\n```\n\n---\n```rescript\ntype Other.t\n```\n"
      }
    }
    |}]

let%expect_test "infers the type of an arithmetic binding" =
  test_hover ~position:(0, 4) ~text:{|let abc = 22 + 34
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]

let%expect_test "shows a tuple type declaration" =
  test_hover ~position:(0, 5) ~text:{|type t = (int, float)
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype t = (int, float)\n```"
      }
    }
    |}]

let%expect_test "shows the signature of a module containing a type" =
  test_hover ~position:(0, 7) ~text:{|module Id = {
  type x = int
}
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nmodule Id: {\n  type x = int\n}\n```"
      }
    }
    |}]

let documented_module =
  {|@res.doc("This module is commented")
module Dep: {
  @res.doc("Some doc comment")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

module D = Dep

let cd = D.customDouble
|}

let%expect_test "shows module documentation at an alias target" =
  test_hover ~text:documented_module ~position:(8, 11);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "\nThis module is commented\n---\n\n```\n \n```\n```rescript\nmodule Dep: {\n  let customDouble: int => int\n}\n```"
      }
    }
    |}]

let%expect_test "shows function documentation through a module alias" =
  test_hover ~text:documented_module ~position:(10, 11);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint => int\n```\n---\nSome doc comment"
      }
    }
    |}]

let%expect_test "shows the annotation and documentation of a function" =
  test_hover ~position:(1, 4)
    ~text:
      {|@res.doc("Doc comment for functionWithTypeAnnotation")
let functionWithTypeAnnotation: unit => int = () => 1
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nunit => int\n```\n---\nDoc comment for functionWithTypeAnnotation"
      }
    }
    |}]

let%expect_test "infers the type of an integer literal" =
  test_hover ~position:(0, 10) ~text:{|let num = 34
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]

let logger_module =
  {|module type Logger = {
  let log: string => unit
}

module JsLogger: Logger = {
  let log = (msg: string) => Console.log(msg)
  let _oneMore = 3
}
|}

let%expect_test "shows a module type signature" =
  test_hover ~text:logger_module ~position:(0, 13);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nmodule type Logger = {\n  let log: string => unit\n}\n```"
      }
    }
    |}]

let%expect_test "shows the constrained signature of a module" =
  test_hover ~text:logger_module ~position:(4, 7);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nmodule type Logger = {\n  let log: string => unit\n}\n```"
      }
    }
    |}]

let%expect_test "shows only the visible bindings of a module with shadowing" =
  test_hover ~position:(0, 9)
    ~text:
      {|module IdDefinedTwice = {
  let _x = 10
  let y = 20
  let _x = 10
}
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nmodule IdDefinedTwice: {\n  let y: int\n  let _x: int\n}\n```"
      }
    }
    |}]

let module_aliases =
  {|module A = {
  let x = 13
}

module B = A

module C = B
|}

let%expect_test "resolves a module alias binding" =
  test_hover ~text:module_aliases ~position:(4, 7);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nmodule A: {\n  let x: int\n}\n```"
      }
    }
    |}]

let%expect_test "resolves a chain of module aliases" =
  test_hover ~text:module_aliases ~position:(6, 7);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nmodule A: {\n  let x: int\n}\n```"
      }
    }
    |}]

let%expect_test "infers a generic record field from a float operation" =
  test_hover ~position:(2, 25)
    ~text:{|type r<'a> = {i: 'a, f: float}

let _get = r => r.f +. r.i
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nfloat\n```" } } |}]

let%expect_test "infers the type of an aliased labeled parameter" =
  test_hover ~position:(0, 21) ~text:{|let withAs = (~xx as yyy) => yyy + 1
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]

let polymorphic_variant_aliases =
  {|module AA = {
  type cond<'a> = [< #str(string)] as 'a
  let fnnxx = (b: cond<_>) => true ? b : b
}

let funAlias = AA.fnnxx

let typeOk = funAlias

let typeDuplicate = AA.fnnxx
|}

let%expect_test
    "preserves polymorphic variant constraints through a function alias" =
  test_hover ~text:polymorphic_variant_aliases ~position:(7, 16);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nAA.cond<([< #str(string)] as 'a)> => AA.cond<'a>\n```\n\n---\n```rescript\ntype AA.cond<'a> = 'a\n  constraint 'a = [< #str(string)]\n```\n"
      }
    }
    |}]

let%expect_test "shows polymorphic variant constraints on a qualified function"
    =
  test_hover ~text:polymorphic_variant_aliases ~position:(9, 25);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nAA.cond<([< #str(string)] as 'a)> => AA.cond<'a>\n```\n\n---\n```rescript\ntype AA.cond<'a> = 'a\n  constraint 'a = [< #str(string)]\n```\n"
      }
    }
    |}]

let%expect_test "shows both unit parameters in a function type" =
  test_hover ~position:(0, 4) ~text:{|let arity0b = ((), ()) => 3
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\n(unit, unit) => int\n```"
      }
    }
    |}]

let%expect_test "shows the type of a function returning a function" =
  test_hover ~position:(0, 5)
    ~text:{|let arity0d = () => {
  let f = () => 3
  f
}
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nunit => unit => int\n```"
      }
    }
    |}]

let%expect_test "includes a compact doc comment on a value" =
  test_hover ~position:(1, 9) ~text:{|/**doc comment 1*/
let docComment1 = 12
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\ndoc comment 1"
      }
    }
    |}]

let%expect_test "preserves spaces in a value doc comment" =
  test_hover ~position:(1, 6)
    ~text:{|/** doc comment 2 */
let docComment2 = 12
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\n doc comment 2 "
      }
    }
    |}]

let record_substitution =
  {|module TypeSubstitutionRecords = {
  type foo<'a> = {content: 'a, zzz: string}
  type bar = {age: int}
  type foobar = foo<bar>

  let x1: foo<bar> = {content: {age: 42}, zzz: ""}
  let x2: foobar = {content: {age: 42}, zzz: ""}
}
|}

let%expect_test "expands the type argument of a generic record" =
  test_hover ~text:record_substitution ~position:(5, 23);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nfoo<bar>\n```\n\n---\n```rescript\ntype foo<'a> = {content: 'a, zzz: string}\n```\n\n\n---\n```rescript\ntype bar = {age: int}\n```\n"
      }
    }
    |}]

let%expect_test "shows the alias of a specialized record" =
  test_hover ~text:record_substitution ~position:(6, 22);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nfoobar\n```\n\n---\n```rescript\ntype foobar = foo<bar>\n```\n"
      }
    }
    |}]

let%expect_test "expands record types nested inside list and option" =
  test_hover ~position:(3, 16)
    ~text:
      {|type r<'a> = {i: 'a, f: float}
type useR = {x: int, y: list<option<r<float>>>}

let testUseR = (v: useR) => v
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nuseR\n```\n\n---\n```rescript\ntype useR = {x: int, y: list<option<r<float>>>}\n```\n\n\n---\n```rescript\ntype r<'a> = {i: 'a, f: float}\n```\n"
      }
    }
    |}]

let%expect_test "uses the documentation of a value rather than its initializer"
    =
  test_hover ~position:(8, 20)
    ~text:
      {|module NotShadowed = {
  /** Stuff */
  let xx_ = 10

  /** More Stuff */
  let xx = xx_
}

let _ = NotShadowed.xx
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\n More Stuff "
      }
    }
    |}]

let%expect_test "uses the documentation of the latest shadowing binding" =
  test_hover ~position:(8, 17)
    ~text:
      {|module Shadowed = {
  /** Stuff */
  let xx = 10

  /** More Stuff */
  let xx = xx
}

let _ = Shadowed.xx
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\n More Stuff "
      }
    }
    |}]

let%expect_test "shows documentation for a record field access" =
  test_hover ~position:(9, 19)
    ~text:
      {|type recordWithDocstringField = {
  /** Mighty fine field here. */
  someField: bool,
}

let x: recordWithDocstringField = {
  someField: true,
}

let someField = x.someField
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nbool\n```\n---\n Mighty fine field here. "
      }
    }
    |}]

let%expect_test "shows a variant constructor with its documentation" =
  test_hover ~position:(2, 20)
    ~text:
      {|type variant = | /** Cool variant! */ CoolVariant | /** Other cool variant */ OtherCoolVariant

let coolVariant = CoolVariant
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nvariant\nCoolVariant\n```\n---\n Cool variant! \n\n---\n```rescript\ntype variant = CoolVariant | OtherCoolVariant\n```\n"
      }
    }
    |}]

let payload_variants =
  {|type payloadVariants = InlineRecord({field1: int, field2: bool}) | Args(int, bool)

let payloadVariant = InlineRecord({field1: 1, field2: true})

let payloadVariant2 = Args(1, true)
|}

let%expect_test "shows a variant constructor with an inline record payload" =
  test_hover ~text:payload_variants ~position:(2, 22);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\npayloadVariants\nInlineRecord({field1: int, field2: bool})\n```\n\n---\n```rescript\ntype payloadVariants =\n  | InlineRecord({field1: int, field2: bool})\n  | Args(int, bool)\n```\n"
      }
    }
    |}]

let%expect_test "shows a variant constructor with positional payloads" =
  test_hover ~text:payload_variants ~position:(4, 23);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\npayloadVariants\nArgs(int, bool)\n```\n\n---\n```rescript\ntype payloadVariants =\n  | InlineRecord({field1: int, field2: bool})\n  | Args(int, bool)\n```\n"
      }
    }
    |}]

let%expect_test "expands a recursive variant from another module" =
  test_hover ~position:(4, 42)
    ~text:
      {|module RecursiveVariants = {
  type rec t = Action1(int) | Action2(float) | Batch(array<t>)
}

let recursiveVariant = RecursiveVariants.Action1(1)
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nRecursiveVariants.t\nAction1(int)\n```\n\n---\n```rescript\ntype RecursiveVariants.t =\n  | Action1(int)\n  | Action2(float)\n  | Batch(array<t>)\n```\n"
      }
    }
    |}]

let%expect_test "expands an alias of a variant type" =
  test_hover ~position:(2, 6)
    ~text:
      {|type variant = CoolVariant | OtherCoolVariant

type aliased = variant
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype aliased = variant\n```\n\n---\n```rescript\ntype variant = CoolVariant | OtherCoolVariant\n```\n"
      }
    }
    |}]

let%expect_test "recognizes res.doc as documentation" =
  test_hover ~position:(0, 32)
    ~text:{|@res.doc("Shown on hover") let resDoc = 1
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\nShown on hover"
      }
    }
    |}]

let%expect_test "ignores doc attributes from another namespace" =
  test_hover ~position:(0, 34)
    ~text:{|@ns.doc("Not shown on hover") let nsDoc = 1
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]

let%expect_test "ignores the legacy ocaml.doc attribute" =
  test_hover ~position:(0, 37)
    ~text:{|@ocaml.doc("Not shown on hover") let ocamlDoc = 1
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]

let%expect_test "returns no hover for a binding inside a line comment" =
  test_hover ~position:(0, 23) ~text:{|// let fff = "hello"; fff
|};
  [%expect {| <Empty> |}]

let%expect_test "returns no hover for a record field inside a line comment" =
  test_hover ~position:(3, 6)
    ~text:
      {|type recordWithDocstringField = {someField: bool}
let x: recordWithDocstringField = {someField: true}

// x.someField
|};
  [%expect {| <Empty> |}]

let%expect_test "preserves fenced examples and nested comments in res.doc" =
  test_hover ~position:(9, 9)
    ~text:
      {|@res.doc("  Doc comment with a triple-backquote example

  ```res example
    let a = 10
    /*
     * stuff
     */
  ```
")
let docComment1 = 12
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\n  Doc comment with a triple-backquote example\n\n  ```res example\n    let a = 10\n    /*\n     * stuff\n     */\n  ```\n"
      }
    }
    |}]

let%expect_test "preserves fenced examples and nested comments in a doc comment"
    =
  test_hover ~position:(10, 6)
    ~text:
      {|/**
  Doc comment with a triple-backquote example

  ```res example
    let a = 10
    /*
     * stuff
     */
  ```
*/
let docComment2 = 12
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nint\n```\n---\n\n  Doc comment with a triple-backquote example\n\n  ```res example\n    let a = 10\n    /*\n     * stuff\n     */\n  ```\n"
      }
    }
    |}]

let%expect_test "resolves a field in an inline record access chain" =
  test_hover ~position:(2, 41)
    ~text:
      {|type person = {details: {name: string}}

let getName = (person: person) => person.details.name
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype person.details = {name: string}\n```"
      }
    }
    |}]

let nested_records =
  {|type options = {
  extra?: {
    name: string,
    superExtra?: {age: int},
    otherExtra: option<{test: bool, anotherInlined: {record: bool}}>,
  },
}

let options = {
  extra: {
    name: "test",
    superExtra: {
      age: 2222,
    },
    otherExtra: Some({
      test: true,
      anotherInlined: {
        record: true,
      },
    }),
  },
}
|}

let%expect_test "infers the outer type of a nested record literal" =
  test_hover ~text:nested_records ~position:(8, 7);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\noptions\n```\n\n---\n```rescript\ntype options = {\n  extra?: {\n    name: string,\n    superExtra?: {age: int},\n    otherExtra: option<\n      {\n        test: bool,\n        anotherInlined: {record: bool},\n      },\n    >,\n  },\n}\n```\n\n\n---\n```rescript\ntype options.extra = {\n  name: string,\n  superExtra?: {age: int},\n  otherExtra: option<\n    {\n      test: bool,\n      anotherInlined: {record: bool},\n    },\n  >,\n}\n```\n"
      }
    }
    |}]

let%expect_test "shows the enclosing inline record at its string field" =
  test_hover ~text:nested_records ~position:(10, 6);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype options.extra = {\n  name: string,\n  superExtra?: {age: int},\n  otherExtra: option<\n    {\n      test: bool,\n      anotherInlined: {record: bool},\n    },\n  >,\n}\n```"
      }
    }
    |}]

let%expect_test "shows the nested optional record at its integer field" =
  test_hover ~text:nested_records ~position:(12, 8);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype options.extra.superExtra = {age: int}\n```"
      }
    }
    |}]

let%expect_test "shows the option-wrapped record at its boolean field" =
  test_hover ~text:nested_records ~position:(15, 9);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype options.extra.otherExtra = {\n  test: bool,\n  anotherInlined: {record: bool},\n}\n```"
      }
    }
    |}]

let%expect_test "shows the deepest inline record at its boolean field" =
  test_hover ~text:nested_records ~position:(17, 11);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\ntype options.extra.otherExtra.anotherInlined = {\n  record: bool,\n}\n```"
      }
    }
    |}]
