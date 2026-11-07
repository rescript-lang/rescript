open Lsp
open Types

let lenses doc =
  Client_request.TextDocumentCodeLens
    (CodeLensParams.create ~textDocument:(Test.text_document doc) ())

let%expect_test "omits generated React component bindings from code lenses" =
  Test.with_project ~name:"code-lens-jsx" ~jsx:true
    ~text:{|@react.component
let make = (~name) => React.string(name)
|}
    (fun ~root ~doc client ->
      Test.configure client (`Assoc [("codeLens", `Bool true)]);
      Test.snapshot ~root client (lenses doc));
  [%expect {| [] |}]

let%expect_test "shows a function type for a dictionary constructor" =
  Test.with_project ~name:"code-lens"
    ~text:{|let make = value => dict{"a": value}
|} (fun ~root ~doc client ->
      Test.configure client (`Assoc [("codeLens", `Bool true)]);
      Test.snapshot ~root client (lenses doc));
  [%expect
    {|
    [
      {
        "command": { "command": "", "title": "'a => dict<'a>" },
        "range": {
          "end": { "character": 8, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "shows a function type for an interpolated template" =
  Test.with_project ~name:"code-lens"
    ~text:{|let greeting = name => `hello ${name}`
|} (fun ~root ~doc client ->
      Test.configure client (`Assoc [("codeLens", `Bool true)]);
      Test.snapshot ~root client (lenses doc));
  [%expect
    {|
    [
      {
        "command": { "command": "", "title": "string => string" },
        "range": {
          "end": { "character": 12, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "does not return code lenses when disabled" =
  Test.with_project ~name:"code-lens" ~text:{|let add = (x, y) => x + y
|}
    (fun ~root ~doc client -> Test.snapshot ~root client (lenses doc));
  [%expect {| [] |}]

let%expect_test "shows types for positional and labeled functions" =
  Test.with_project ~name:"code-lens"
    ~text:
      {|let add = (x, y) => x + y
let foo = (~age, ~name) => name ++ Int.toString(age)
|}
    (fun ~root ~doc client ->
      Test.configure client (`Assoc [("codeLens", `Bool true)]);
      Test.snapshot ~root client (lenses doc));
  [%expect
    {|
    [
      {
        "command": {
          "command": "",
          "title": "(~age: int, ~name: string) => string"
        },
        "range": {
          "end": { "character": 7, "line": 1 },
          "start": { "character": 4, "line": 1 }
        }
      },
      {
        "command": { "command": "", "title": "(int, int) => int" },
        "range": {
          "end": { "character": 7, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
    ]
    |}]

let%expect_test "shows a type lens for a braced function" =
  Test.with_project ~name:"code-lens"
    ~text:{|let bracedAdd = {(x, y) => x + y}
|} (fun ~root ~doc client ->
      Test.configure client (`Assoc [("codeLens", `Bool true)]);
      Test.snapshot ~root client (lenses doc));
  [%expect
    {|
    [
      {
        "command": { "command": "", "title": "(int, int) => int" },
        "range": {
          "end": { "character": 13, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
    ]
    |}]
