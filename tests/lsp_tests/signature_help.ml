open Lsp
open Types

let signature doc pos =
  Client_request.SignatureHelp
    (SignatureHelpParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ())

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
