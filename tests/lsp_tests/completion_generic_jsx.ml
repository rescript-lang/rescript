let generic_jsx =
  {|type element = Jsx.element
type component<'props> = Jsx.component<'props>
type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
external component: componentLike<'props, element> => component<'props> = "%component_identity"
@module("preact") external jsx: (component<'props>, 'props) => element = "jsx"
@module("preact") external jsxKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsx"
@module("preact") external jsxs: (component<'props>, 'props) => element = "jsxs"
@module("preact") external jsxsKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsxs"
external array: array<element> => element = "%identity"
@val external null: element = "null"
external string: string => element = "%identity"
module Elements = {
  type props = {testing?: bool, test2?: string, children?: element}
  @module("preact") external jsx: (string, props) => element = "jsx"
  @module("preact") external jsxKeyed: (string, props, ~key: string=?, @ignore unit) => element = "jsx"
  @module("preact") external jsxs: (string, props) => element = "jsxs"
  @module("preact") external jsxsKeyed: (string, props, ~key: string=?, @ignore unit) => element = "jsxs"
}
|}

let with_project ~text f =
  Test.with_project ~name:"generic-jsx" ~text
    ~rescript_json:
      {|{"name":"generic-jsx","sources":{"dir":"."},"jsx":{"module":"GenericJsx"}}|}
    ~files:[("GenericJsx.res", generic_jsx)]
    f

let%expect_test "completes intrinsic props from a custom JSX runtime" =
  with_project ~text:{|let _ = GenericJsx.null
|} (fun ~root ~doc client ->
      Test.change_document client doc ~version:1 ~text:{|let _ = <div |};
      Test.snapshot ~root client (Completion.completion doc (0, 13)));
  [%expect
    {|
    [
      { "detail": "bool", "kind": 4, "label": "testing", "tags": [] },
      { "detail": "string", "kind": 4, "label": "test2", "tags": [] },
      { "detail": "element", "kind": 4, "label": "children", "tags": [] }
    ]
    |}]

let%expect_test "completes boolean values for a custom JSX intrinsic prop" =
  with_project ~text:{|let _ = GenericJsx.null
|} (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:{|let _ = <div testing={}|};
      Test.snapshot ~root client (Completion.completion doc (0, 22)));
  [%expect
    {|
    [
      { "detail": "bool", "kind": 4, "label": "true", "tags": [] },
      { "detail": "bool", "kind": 4, "label": "false", "tags": [] }
    ]
    |}]

let component_prefix =
  {|module SomeComponent = {
  @jsx.component
  let make = (~someProp) => {
    let someString = ""
|}

let component =
  component_prefix ^ {|    GenericJsx.string(someProp ++ someString)
  }
}
|}

let%expect_test "offers a custom JSX string conversion in a component body" =
  with_project ~text:component (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(component_prefix ^ {|    someString->st
  }
}
|});
      Test.snapshot ~root client (Completion.completion doc (4, 18)));
  [%expect
    {|
    [
      {
        "detail": "string",
        "documentation": {
          "kind": "markdown",
          "value": "Turns `string` into a JSX element so it can be used inside of JSX."
        },
        "insertTextFormat": 2,
        "kind": 12,
        "label": "GenericJsx.string",
        "sortText": "A",
        "tags": []
      },
      {
        "detail": "(string, string) => bool",
        "documentation": {
          "kind": "markdown",
          "value": "\n`startsWith(str, substr)` returns `true` if the `str` starts with `substr`,\n`false` otherwise.\nSee [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith) on MDN.\n\n## Examples\n\n```rescript\nString.startsWith(\"BuckleScript\", \"Buckle\") == true\nString.startsWith(\"BuckleScript\", \"\") == true\nString.startsWith(\"JavaScript\", \"Buckle\") == false\n```\n"
        },
        "kind": 12,
        "label": "String.startsWith",
        "tags": []
      },
      {
        "detail": "(string, string, int) => bool",
        "documentation": {
          "kind": "markdown",
          "value": "\n`startsWithFrom(str, substr, n)` returns `true` if the `str` starts\nwith `substr` starting at position `n`, `false` otherwise. If `n` is negative,\nthe search starts at the beginning of `str`.\nSee [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith) on MDN.\n\n## Examples\n\n```rescript\nString.startsWithFrom(\"BuckleScript\", \"kle\", 3) == true\nString.startsWithFrom(\"BuckleScript\", \"\", 3) == true\nString.startsWithFrom(\"JavaScript\", \"Buckle\", 2) == false\n```\n"
        },
        "kind": 12,
        "label": "String.startsWithFrom",
        "tags": []
      }
    ]
    |}]

let%expect_test "offers a custom JSX string conversion with the runtime open" =
  with_project ~text:component (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:
          (component_prefix
         ^ {|    open GenericJsx
    <div>
      {someString->st}
    </div>
  }
}
|}
          );
      Test.snapshot ~root client (Completion.completion doc (6, 21)));
  [%expect
    {|
    [
      {
        "detail": "string",
        "documentation": {
          "kind": "markdown",
          "value": "Turns `string` into a JSX element so it can be used inside of JSX."
        },
        "insertTextFormat": 2,
        "kind": 12,
        "label": "string",
        "sortText": "A",
        "tags": []
      },
      {
        "detail": "(string, string) => bool",
        "documentation": {
          "kind": "markdown",
          "value": "\n`startsWith(str, substr)` returns `true` if the `str` starts with `substr`,\n`false` otherwise.\nSee [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith) on MDN.\n\n## Examples\n\n```rescript\nString.startsWith(\"BuckleScript\", \"Buckle\") == true\nString.startsWith(\"BuckleScript\", \"\") == true\nString.startsWith(\"JavaScript\", \"Buckle\") == false\n```\n"
        },
        "kind": 12,
        "label": "String.startsWith",
        "tags": []
      },
      {
        "detail": "(string, string, int) => bool",
        "documentation": {
          "kind": "markdown",
          "value": "\n`startsWithFrom(str, substr, n)` returns `true` if the `str` starts\nwith `substr` starting at position `n`, `false` otherwise. If `n` is negative,\nthe search starts at the beginning of `str`.\nSee [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith) on MDN.\n\n## Examples\n\n```rescript\nString.startsWithFrom(\"BuckleScript\", \"kle\", 3) == true\nString.startsWithFrom(\"BuckleScript\", \"\", 3) == true\nString.startsWithFrom(\"JavaScript\", \"Buckle\", 2) == false\n```\n"
        },
        "kind": 12,
        "label": "String.startsWithFrom",
        "tags": []
      }
    ]
    |}]
