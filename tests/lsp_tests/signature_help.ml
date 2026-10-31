open Lsp
open Types

let signature doc pos =
  Client_request.SignatureHelp
    (SignatureHelpParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

let%expect_test "selects an optional labeled argument" =
  Test.with_project ~name:"signature-help"
    ~text:
      {|let someFunc = (one: int, ~two: option<string>=?, ()) => {
  ignore(one)
  ignore(two)
}
let _ = someFunc(123, ~two="123", ())
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (signature doc (4, 28)));
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 1,
          "label": "(int, ~two: string=?, unit) => unit",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 1, 4 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 7, 20 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 22, 26 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "accounts for the piped argument when selecting a parameter" =
  Test.with_project ~name:"signature-help"
    ~text:
      {|let otherFunc = (first: string, second: int, third: float) => {
  ignore(first)
  ignore(second)
  ignore(third)
}
let _ = "hello"->otherFunc(1, 2.0)
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (signature doc (5, 26)));
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 1,
          "label": "(string, int, float) => unit",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 1, 7 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 9, 12 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 14, 19 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "distinguishes a constructor with a single tuple payload" =
  Test.with_project ~name:"signature-help"
    ~text:{|type unary = Unary((int, string))
let unary = Unary(1, "test")
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (signature doc (1, 22)));
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 0,
          "label": "Unary((int, string))",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 6, 19 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "shows constructor signature help in a destructuring pattern" =
  Test.with_project ~name:"signature-help"
    ~text:
      {|type t = Pair(int, string)
let read = value => switch value {
| Pair(first, second) => second
}
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (signature doc (2, 15)));
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 1,
          "label": "Pair(int, string)",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 5, 8 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 10, 16 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "selects the next constructor argument in the gap after a comma"
    =
  Test.with_project ~name:"signature-help"
    ~text:{|type t = Three(string, array<int>)
let value = Three("",    [])
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (signature doc (1, 22)));
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 1,
          "label": "Three(string, array<int>)",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 6, 12 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 14, 24 ]
            }
          ]
        }
      ]
    }
    |}]

(* TODO: Recovery for an omitted constructor argument currently returns no
   signature, unlike the whitespace gap before an existing argument above. *)
let%expect_test "returns no signature for a missing constructor payload" =
  let text =
    {|type t = Three(string, array<int>)
let value = Three("", [])
|}
  in
  Test.with_project ~name:"signature-help" ~text (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let next = Three("", )|});
      Test.snapshot ~root client (signature doc (2, 21)));
  [%expect {| { "signatures": [] } |}]

let function_source =
  {|let otherFunc = (first: string, second: int, third: float) => {
  ignore(first)
  ignore(second)
  ignore(third)
}
|}

let%expect_test "shows the first parameter of an incomplete function call" =
  Test.with_project ~name:"signature-help" ~text:function_source
    (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(function_source ^ {|let _ = otherFunc(|});
      Test.snapshot ~root client (signature doc (5, 18)));
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 0,
          "label": "(string, int, float) => unit",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 1, 7 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 9, 12 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 14, 19 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "selects the second parameter after a comma" =
  Test.with_project ~name:"signature-help" ~text:function_source
    (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(function_source ^ {|let _ = otherFunc("123", 123, 123.0)
|});
      Test.snapshot ~root client (signature doc (5, 25)));
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 1,
          "label": "(string, int, float) => unit",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 1, 7 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 9, 12 ]
            },
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 14, 19 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "shows a documented constructor payload signature" =
  Test.with_project ~name:"signature-help"
    ~text:{|type t =
  | /** Two is fun! */ Two(string)
let two = Two("true")
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (signature doc (2, 14)));
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "activeParameter": 0,
          "documentation": { "kind": "markdown", "value": " Two is fun! " },
          "label": "Two(string)",
          "parameters": [
            {
              "documentation": { "kind": "markdown", "value": "" },
              "label": [ 4, 10 ]
            }
          ]
        }
      ]
    }
    |}]

let%expect_test "returns no signatures outside a call" =
  Test.with_project ~name:"signature-help" ~text:{|let x = 12
|}
    (fun ~root ~doc client -> Test.snapshot ~root client (signature doc (0, 4)));
  [%expect {| { "signatures": [] } |}]
