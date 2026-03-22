// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Frontend — AST transformations
// Tests PPX, decorators, external declarations, pipe desugaring, and other
// frontend transformations by verifying runtime behavior.

let eq = (a, b) => a == b
let approxEq = (a, b) => Float.Math.abs(a -. b) < 0.0001

// ─── @module imports ────────────────────────────────────────────────
@module("path") @val external pathJoin: (string, string) => string = "join"
// Note: this binds to the path.join function from Node.js
let joined = pathJoin("a", "b")
Test.run(__POS_OF__("@module: path.join"), String.includes(joined, "b"), eq, true)

// ─── @val global bindings ───────────────────────────────────────────
@val external parseInt: (string, int) => int = "parseInt"
Test.run(__POS_OF__("@val: parseInt decimal"), parseInt("42", 10), eq, 42)
Test.run(__POS_OF__("@val: parseInt hex"), parseInt("ff", 16), eq, 255)
Test.run(__POS_OF__("@val: parseInt binary"), parseInt("1010", 2), eq, 10)

@val external parseFloat: string => float = "parseFloat"
Test.run(__POS_OF__("@val: parseFloat"), parseFloat("3.14") > 3.13, eq, true)

@val external isNaN: float => bool = "isNaN"
Test.run(__POS_OF__("@val: isNaN true"), isNaN(nan), eq, true)
Test.run(__POS_OF__("@val: isNaN false"), isNaN(1.0), eq, false)

@val external isFinite: float => bool = "isFinite"
Test.run(__POS_OF__("@val: isFinite number"), isFinite(42.0), eq, true)
Test.run(__POS_OF__("@val: isFinite inf"), isFinite(infinity), eq, false)

// ─── @scope for namespaced globals ──────────────────────────────────
@scope("Math") @val external mathMax: (float, float) => float = "max"
@scope("Math") @val external mathMin: (float, float) => float = "min"
@scope("Math") @val external mathFloor: float => float = "floor"
@scope("Math") @val external mathCeil: float => float = "ceil"
@scope("Math") @val external mathRound: float => float = "round"
@scope("Math") @val external mathAbs: float => float = "abs"

Test.run(__POS_OF__("@scope: Math.max"), mathMax(3.0, 7.0), approxEq, 7.0)
Test.run(__POS_OF__("@scope: Math.min"), mathMin(3.0, 7.0), approxEq, 3.0)
Test.run(__POS_OF__("@scope: Math.floor"), mathFloor(3.7), approxEq, 3.0)
Test.run(__POS_OF__("@scope: Math.ceil"), mathCeil(3.2), approxEq, 4.0)
Test.run(__POS_OF__("@scope: Math.round"), mathRound(3.5), approxEq, 4.0)
Test.run(__POS_OF__("@scope: Math.abs"), mathAbs(-5.0), approxEq, 5.0)

// ─── @send for method calls ────────────────────────────────────────
@send external toUpperCase: string => string = "toUpperCase"
@send external toLowerCase: string => string = "toLowerCase"
@send external trim: string => string = "trim"
@send external jsSlice: (string, int, int) => string = "slice"

Test.run(__POS_OF__("@send: toUpperCase"), "hello"->toUpperCase, eq, "HELLO")
Test.run(__POS_OF__("@send: toLowerCase"), "HELLO"->toLowerCase, eq, "hello")
Test.run(__POS_OF__("@send: trim"), "  hi  "->trim, eq, "hi")
Test.run(__POS_OF__("@send: slice"), "hello"->jsSlice(1, 4), eq, "ell")

// ─── @get and @set for property access ──────────────────────────────
type jsObj = {mutable length: int}
@get external getLength: array<'a> => int = "length"
Test.run(__POS_OF__("@get: array length"), getLength([1, 2, 3]), eq, 3)
Test.run(__POS_OF__("@get: empty array"), getLength([]), eq, 0)

// ─── @new for constructors ──────────────────────────────────────────
@new external createDate: unit => {..} = "Date"
let d = createDate()
Test.run(__POS_OF__("@new: Date created"), true, eq, true) // Just verifying it doesn't crash

@new external createArray: int => array<int> = "Array"
let emptyArr = createArray(3)
Test.run(__POS_OF__("@new: Array constructor"), getLength(emptyArr), eq, 3)

// ─── @variadic for rest params ──────────────────────────────────────
@variadic @scope("Math") @val external mathMaxMany: array<float> => float = "max"
Test.run(
  __POS_OF__("@variadic: Math.max many"),
  mathMaxMany([1.0, 5.0, 3.0, 7.0, 2.0]),
  approxEq,
  7.0,
)
Test.run(
  __POS_OF__("@variadic: Math.max single"),
  mathMaxMany([42.0]),
  approxEq,
  42.0,
)

// ─── %raw escape hatch ─────────────────────────────────────────────
let rawResult: int = %raw(`1 + 2 + 3`)
Test.run(__POS_OF__("%raw: expression"), rawResult, eq, 6)

let rawFn: (int, int) => int = %raw(`(a, b) => a * b + 1`)
Test.run(__POS_OF__("%raw: function"), rawFn(3, 4), eq, 13)

// ─── Pipe operator desugaring ───────────────────────────────────────
// The pipe operator is transformed in the frontend to a function application
let pipeResult = 5->Int.toString
Test.run(__POS_OF__("pipe: simple"), pipeResult, eq, "5")

let pipeChain = [1, 2, 3]->Array.map(x => x * 2)->Array.reduce(0, (a, b) => a + b)
Test.run(__POS_OF__("pipe: chain"), pipeChain, eq, 12)

let pipeLambda = 10->(x => x + 5)
Test.run(__POS_OF__("pipe: lambda"), pipeLambda, eq, 15)

// Pipe with labeled args
let pipeLabeled = [3, 1, 2]->Array.toSorted((a, b) => Float.fromInt(a - b))
Test.run(__POS_OF__("pipe: with sorted"), pipeLabeled, eq, [1, 2, 3])

// ─── String interpolation desugaring ────────────────────────────────
// String interpolation is desugared in the frontend
let interpSimple = {
  let x = "world"
  `hello ${x}`
}
Test.run(__POS_OF__("interp: simple desugar"), interpSimple, eq, "hello world")

let interpMulti = {
  let a = "one"
  let b = "two"
  let c = "three"
  `${a}, ${b}, ${c}`
}
Test.run(__POS_OF__("interp: multi desugar"), interpMulti, eq, "one, two, three")

let interpNested = `${"inner" ++ " " ++ "concat"}`
Test.run(__POS_OF__("interp: with concat"), interpNested, eq, "inner concat")

// ─── Deriving ───────────────────────────────────────────────────────
// accessors via @deriving
@deriving(accessors)
type colorRgb = {r: int, g: int, b: int}

let c = {r: 255, g: 128, b: 0}
Test.run(__POS_OF__("deriving: accessor r"), r(c), eq, 255)
Test.run(__POS_OF__("deriving: accessor g"), g(c), eq, 128)
Test.run(__POS_OF__("deriving: accessor b"), b(c), eq, 0)

// ─── @as for renaming ──────────────────────────────────────────────
type buttonProps = {
  @as("type") type_: string,
  @as("class") className: string,
}
let btn: buttonProps = {type_: "submit", className: "primary"}
Test.run(__POS_OF__("@as: renamed field type"), btn.type_, eq, "submit")
Test.run(__POS_OF__("@as: renamed field class"), btn.className, eq, "primary")

// ─── Pattern match compilation ──────────────────────────────────────
// Frontend compiles pattern matches into decision trees
type animal = Cat | Dog | Bird | Fish | Lizard | Snake | Turtle

let legs = a =>
  switch a {
  | Cat | Dog => 4
  | Bird => 2
  | Fish => 0
  | Lizard => 4
  | Snake => 0
  | Turtle => 4
  }

Test.run(__POS_OF__("pattern: variant cat"), legs(Cat), eq, 4)
Test.run(__POS_OF__("pattern: variant bird"), legs(Bird), eq, 2)
Test.run(__POS_OF__("pattern: variant snake"), legs(Snake), eq, 0)

// Complex nested patterns
type coord = {cx: int, cy: int}
type element = {pos: coord, tag: string}

let classify = elem =>
  switch elem {
  | {pos: {cx: 0, cy: 0}, tag: "origin"} => "labeled origin"
  | {pos: {cx: 0, cy: 0}} => "origin"
  | {pos: {cx: 0}} => "y-axis"
  | {pos: {cy: 0}} => "x-axis"
  | {tag: "special"} => "special"
  | _ => "general"
  }

Test.run(
  __POS_OF__("pattern: nested labeled origin"),
  classify({pos: {cx: 0, cy: 0}, tag: "origin"}),
  eq,
  "labeled origin",
)
Test.run(
  __POS_OF__("pattern: nested origin"),
  classify({pos: {cx: 0, cy: 0}, tag: "other"}),
  eq,
  "origin",
)
Test.run(
  __POS_OF__("pattern: nested y-axis"),
  classify({pos: {cx: 0, cy: 5}, tag: ""}),
  eq,
  "y-axis",
)
Test.run(
  __POS_OF__("pattern: nested x-axis"),
  classify({pos: {cx: 3, cy: 0}, tag: ""}),
  eq,
  "x-axis",
)
Test.run(
  __POS_OF__("pattern: nested special"),
  classify({pos: {cx: 1, cy: 1}, tag: "special"}),
  eq,
  "special",
)

// ─── Uncurrying transformation ──────────────────────────────────────
// The frontend transforms curried functions
let uncurriedAdd = (a, b) => a + b
Test.run(__POS_OF__("uncurry: simple"), uncurriedAdd(3, 4), eq, 7)

let uncurriedThree = (a, b, c) => a + b + c
Test.run(__POS_OF__("uncurry: three args"), uncurriedThree(1, 2, 3), eq, 6)

// ─── Module functor application ─────────────────────────────────────
module type Comparable = {
  type t
  let compare: (t, t) => int
}

module MakeSet = (C: Comparable) => {
  type t = array<C.t>
  let empty: t = []
  let has = (set: t, item: C.t) => set->Array.some(x => C.compare(x, item) == 0)
  let add = (set: t, item: C.t) =>
    if has(set, item) {
      set
    } else {
      Array.concat(set, [item])
    }
  let size = (set: t) => Array.length(set)
}

module IntSet = MakeSet({
  type t = int
  let compare = (a, b) => a - b
})

let s = IntSet.empty->IntSet.add(1)->IntSet.add(2)->IntSet.add(1)
Test.run(__POS_OF__("functor: set size"), IntSet.size(s), eq, 2)
Test.run(__POS_OF__("functor: set has 1"), IntSet.has(s, 1), eq, true)
Test.run(__POS_OF__("functor: set has 2"), IntSet.has(s, 2), eq, true)
Test.run(__POS_OF__("functor: set not has 3"), IntSet.has(s, 3), eq, false)
