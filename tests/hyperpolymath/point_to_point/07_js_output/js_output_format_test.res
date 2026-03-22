// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: JS Output — module format and output quality
// Tests that JavaScript output correctly handles imports, exports, and
// module system features.

let eq = (a, b) => a == b

// ─── Module export correctness ──────────────────────────────────────
// All top-level let bindings should be exportable
let exportedInt = 42
let exportedString = "hello"
let exportedBool = true
let exportedArray = [1, 2, 3]

Test.run(__POS_OF__("export: int"), exportedInt, eq, 42)
Test.run(__POS_OF__("export: string"), exportedString, eq, "hello")
Test.run(__POS_OF__("export: bool"), exportedBool, eq, true)
Test.run(__POS_OF__("export: array"), exportedArray, eq, [1, 2, 3])

// Exported functions
let exportedFn = (a, b) => a + b
Test.run(__POS_OF__("export: function"), exportedFn(3, 4), eq, 7)

// ─── Module system: nested modules ──────────────────────────────────
module Outer = {
  let outerVal = 1
  module Middle = {
    let middleVal = 2
    module Inner = {
      let innerVal = 3
    }
  }
}

Test.run(__POS_OF__("module: outer"), Outer.outerVal, eq, 1)
Test.run(__POS_OF__("module: middle"), Outer.Middle.middleVal, eq, 2)
Test.run(__POS_OF__("module: inner"), Outer.Middle.Inner.innerVal, eq, 3)

// ─── Module open ────────────────────────────────────────────────────
module MyMath = {
  let pi = 3.14159
  let e = 2.71828
  let square = x => x *. x
  let cube = x => x *. x *. x
}

let circleArea = {
  open MyMath
  pi *. square(5.0)
}
Test.run(__POS_OF__("module open: scoped"), circleArea > 78.0, eq, true)

// ─── Module aliases ─────────────────────────────────────────────────
module M = MyMath
Test.run(__POS_OF__("module alias: pi"), M.pi > 3.14, eq, true)
Test.run(__POS_OF__("module alias: square"), M.square(4.0) > 15.9, eq, true)

// ─── Module include ─────────────────────────────────────────────────
module Extended = {
  include MyMath
  let tau = pi *. 2.0
}
Test.run(__POS_OF__("module include: inherited"), Extended.pi > 3.14, eq, true)
Test.run(__POS_OF__("module include: new"), Extended.tau > 6.28, eq, true)

// ─── Side-effect free modules ───────────────────────────────────────
// Pure modules with only type definitions and pure functions
module PureTypes = {
  type color = Red | Green | Blue
  let colorToString = c =>
    switch c {
    | Red => "red"
    | Green => "green"
    | Blue => "blue"
    }
}
Test.run(
  __POS_OF__("pure module: type+fn"),
  PureTypes.colorToString(PureTypes.Red),
  eq,
  "red",
)

// ─── Recursive module patterns ──────────────────────────────────────
module type NodeType = {
  type t
  let make: (int, array<t>) => t
  let value: t => int
  let children: t => array<t>
}

module TreeNode: NodeType = {
  type t = {value: int, children: array<t>}
  let make = (v, c) => {value: v, children: c}
  let value = n => n.value
  let children = n => n.children
}

let root = TreeNode.make(1, [TreeNode.make(2, []), TreeNode.make(3, [])])
Test.run(__POS_OF__("recursive module: root"), TreeNode.value(root), eq, 1)
Test.run(
  __POS_OF__("recursive module: child"),
  TreeNode.value(TreeNode.children(root)[0]),
  eq,
  2,
)

// ─── Import deduplication ───────────────────────────────────────────
// Multiple uses of the same module should not duplicate imports
let r1 = Array.length([1, 2, 3])
let r2 = Array.length([4, 5])
let r3 = Array.map([1, 2], x => x + 1)
let r4 = Array.filter([1, 2, 3], x => x > 1)
Test.run(__POS_OF__("import dedup: length 1"), r1, eq, 3)
Test.run(__POS_OF__("import dedup: length 2"), r2, eq, 2)
Test.run(__POS_OF__("import dedup: map"), r3, eq, [2, 3])
Test.run(__POS_OF__("import dedup: filter"), r4, eq, [2, 3])

// ─── Export naming correctness ──────────────────────────────────────
// Names with special characters are properly escaped
let let_ = 42
let type_ = "type"
let switch_ = true
Test.run(__POS_OF__("export name: let_"), let_, eq, 42)
Test.run(__POS_OF__("export name: type_"), type_, eq, "type")
Test.run(__POS_OF__("export name: switch_"), switch_, eq, true)

// ─── Circular module reference patterns ─────────────────────────────
// These are valid in ReScript and should produce correct JS
module A2 = {
  let x = 1
  let f = () => x + 1
}

module B2 = {
  let y = A2.x + 10
  let g = () => A2.f() + y
}

Test.run(__POS_OF__("circular: A.x"), A2.x, eq, 1)
Test.run(__POS_OF__("circular: B.y"), B2.y, eq, 11)
Test.run(__POS_OF__("circular: B.g"), B2.g(), eq, 13)

// ─── Generated code quality ─────────────────────────────────────────
// These test that certain patterns produce efficient JS
// (verified by runtime correctness — JS quality is validated by snapshot tests)

// Simple identity should be lightweight
let identity = x => x
Test.run(__POS_OF__("quality: identity"), identity(42), eq, 42)

// Map-filter-reduce chain should work
let pipeline =
  Array.fromInitializer(~length=100, i => i + 1)
  ->Array.filter(x => x mod 2 == 0)
  ->Array.map(x => x * x)
  ->Array.reduce(0, (a, b) => a + b)
Test.run(__POS_OF__("quality: pipeline"), pipeline, eq, 171700)

// Tail-recursive function should not stack overflow
let rec sumTail = (n, acc) =>
  if n <= 0 {
    acc
  } else {
    sumTail(n - 1, acc + n)
  }
Test.run(__POS_OF__("quality: tail recursion"), sumTail(10000, 0), eq, 50005000)
