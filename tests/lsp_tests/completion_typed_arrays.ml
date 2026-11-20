let test_completion ~text ~receiver =
  Test.with_project ~name:"typed-array-completion" ~text
    (fun ~root ~doc client ->
      (* Use one shared method prefix to check inferred receiver lookup and
         element types without repeating every method's documentation. *)
      let expression = receiver ^ ".findLastIndexWithIndex" in
      Test.change_document client doc ~version:1 ~text:(text ^ expression);
      Test.snapshot ~root client
        (Completion.completion doc (1, String.length expression)))

let%expect_test "completes methods on a BigInt64Array receiver" =
  test_completion ~text:{|let value = BigInt64Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (bigint, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->BigInt64Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->BigInt64Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a BigUint64Array receiver" =
  test_completion ~text:{|let value = BigUint64Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (bigint, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->BigUint64Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->BigUint64Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a Float32Array receiver" =
  test_completion ~text:{|let value = Float32Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (float, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Float32Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Float32Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a Float64Array receiver" =
  test_completion ~text:{|let value = Float64Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (float, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Float64Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Float64Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on an Int16Array receiver" =
  test_completion ~text:{|let value = Int16Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Int16Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Int16Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on an Int32Array receiver" =
  test_completion ~text:{|let value = Int32Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Int32Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Int32Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on an Int8Array receiver" =
  test_completion ~text:{|let value = Int8Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Int8Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Int8Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a Uint16Array receiver" =
  test_completion ~text:{|let value = Uint16Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Uint16Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Uint16Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a Uint32Array receiver" =
  test_completion ~text:{|let value = Uint32Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Uint32Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Uint32Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a Uint8Array receiver" =
  test_completion ~text:{|let value = Uint8Array.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Uint8Array.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Uint8Array.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]

let%expect_test "completes methods on a Uint8ClampedArray receiver" =
  test_completion ~text:{|let value = Uint8ClampedArray.fromLength(0)
|}
    ~receiver:"value";
  [%expect
    {|
    [
      {
        "additionalTextEdits": [
          {
            "newText": "",
            "range": {
              "end": { "character": 6, "line": 1 },
              "start": { "character": 5, "line": 1 }
            }
          }
        ],
        "detail": "(t, (int, int) => bool) => int",
        "documentation": {
          "kind": "markdown",
          "value": "\n  `findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.\n  "
        },
        "insertText": "->Uint8ClampedArray.findLastIndexWithIndex",
        "kind": 12,
        "label": "->Uint8ClampedArray.findLastIndexWithIndex",
        "sortText": "findLastIndexWithIndex",
        "tags": []
      }
    ]
    |}]
