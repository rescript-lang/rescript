open Lsp
open Types

let rename doc pos new_name =
  Client_request.TextDocumentRename
    (RenameParams.create ~textDocument:(Test.text_document doc)
       ~position:(Test.position pos) ~newName:new_name ())

(* TODO: The newUri values are currently double encoded. This snapshot records
   the defect rather than normalizing it into a valid destination URI. *)
let%expect_test "records file renames and qualified edits for a source module" =
  Test.with_project ~name:"rename"
    ~text:{|let first = Other.x
let second = Other.x
|}
    ~files:[("Other.res", {|let x = 12
|}); ("Other.resi", {|let x: int
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 13) "Renamed"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "kind": "rename",
          "newUri": "file:///file%3A///workspace/./Renamed.resi",
          "oldUri": "file:///workspace/./Other.resi"
        },
        {
          "kind": "rename",
          "newUri": "file:///file%3A///workspace/./Renamed.res",
          "oldUri": "file:///workspace/./Other.res"
        },
        {
          "edits": [
            {
              "newText": "Renamed",
              "range": {
                "end": { "character": 17, "line": 0 },
                "start": { "character": 12, "line": 0 }
              }
            },
            {
              "newText": "Renamed",
              "range": {
                "end": { "character": 18, "line": 1 },
                "start": { "character": 13, "line": 1 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

let%expect_test "renames a pattern binding without renaming the record field" =
  Test.with_project ~name:"rename"
    ~text:
      {|type person = {name: string}
let person = {name: "Alice"}
let {name: localName} = person
let greeting = localName
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (2, 15) "displayName"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "displayName",
              "range": {
                "end": { "character": 20, "line": 2 },
                "start": { "character": 11, "line": 2 }
              }
            },
            {
              "newText": "displayName",
              "range": {
                "end": { "character": 24, "line": 3 },
                "start": { "character": 15, "line": 3 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

(* TODO: Renaming the module currently omits its JSX tag references. *)
let%expect_test
    "records current rename edits for a component module used in JSX" =
  Test.with_project ~name:"rename-jsx" ~jsx:true
    ~text:
      {|module Comp = {
  @react.component
  let make = () => React.null
}
let first = <Comp />
let second = <Comp />
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 8) "Panel"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "Panel",
              "range": {
                "end": { "character": 11, "line": 0 },
                "start": { "character": 7, "line": 0 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

let%expect_test "renames a value and all local uses" =
  Test.with_project ~name:"rename" ~text:{|let x = 12
let a = x
let c = x
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 4) "y"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 5, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            },
            {
              "newText": "y",
              "range": {
                "end": { "character": 9, "line": 1 },
                "start": { "character": 8, "line": 1 }
              }
            },
            {
              "newText": "y",
              "range": {
                "end": { "character": 9, "line": 2 },
                "start": { "character": 8, "line": 2 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

let%expect_test "renames a labeled parameter and its use" =
  Test.with_project ~name:"rename" ~text:{|let foo = (~xx) => xx + 1
|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 19) "yy"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "yy",
              "range": {
                "end": { "character": 14, "line": 0 },
                "start": { "character": 11, "line": 0 }
              }
            },
            {
              "newText": "yy",
              "range": {
                "end": { "character": 21, "line": 0 },
                "start": { "character": 19, "line": 0 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        }
      ]
    }
    |}]

let%expect_test "renames an export in its interface and consumer" =
  Test.with_project ~name:"rename" ~text:{|let x = 12
|}
    ~files:
      [("Test.resi", {|let x: int
|}); ("Consumer.res", {|let a = Test.x
|})]
    (fun ~root ~doc client ->
      Test.snapshot ~root client (rename doc (0, 4) "y"));
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 5, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            }
          ],
          "textDocument": { "uri": "file:///workspace/Test.res", "version": 0 }
        },
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 5, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            }
          ],
          "textDocument": {
            "uri": "file:///workspace/./Test.resi",
            "version": 0
          }
        },
        {
          "edits": [
            {
              "newText": "y",
              "range": {
                "end": { "character": 14, "line": 0 },
                "start": { "character": 13, "line": 0 }
              }
            }
          ],
          "textDocument": {
            "uri": "file:///workspace/./Consumer.res",
            "version": 0
          }
        }
      ]
    }
    |}]
