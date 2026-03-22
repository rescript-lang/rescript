// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Parser — expression parsing
// Tests that all expression forms parse correctly and produce correct runtime
// values after full compilation.

let eq = (a, b) => a == b

// ─── Let bindings ───────────────────────────────────────────────────
// Simple let
let simpleVal = 42
Test.run(__POS_OF__("let: simple binding"), simpleVal, eq, 42)

// Let with type annotation
let annotated: int = 100
Test.run(__POS_OF__("let: annotated"), annotated, eq, 100)

// Destructured let — tuple
let (da, db) = (10, 20)
Test.run(__POS_OF__("let: destructure tuple a"), da, eq, 10)
Test.run(__POS_OF__("let: destructure tuple b"), db, eq, 20)

// Destructured let — record
type namedPair = {first: int, second: string}
let {first, second} = {first: 1, second: "two"}
Test.run(__POS_OF__("let: destructure record first"), first, eq, 1)
Test.run(__POS_OF__("let: destructure record second"), second, eq, "two")

// Destructured let — array
let [head, ..._rest] = [1, 2, 3]
Test.run(__POS_OF__("let: destructure array head"), head, eq, 1)

// ─── Function definitions ───────────────────────────────────────────
// Arrow function
let add = (a, b) => a + b
Test.run(__POS_OF__("fn: arrow"), add(2, 3), eq, 5)

// Single arg arrow (no parens)
let double = x => x * 2
Test.run(__POS_OF__("fn: single arg arrow"), double(5), eq, 10)

// Labeled arguments
let greet = (~greeting, ~name) => greeting ++ " " ++ name
Test.run(__POS_OF__("fn: labeled args"), greet(~greeting="Hello", ~name="World"), eq, "Hello World")

// Optional arguments
let optGreet = (~greeting="Hi", name) => greeting ++ " " ++ name
Test.run(__POS_OF__("fn: optional with default"), optGreet("Alice"), eq, "Hi Alice")
Test.run(
  __POS_OF__("fn: optional overridden"),
  optGreet(~greeting="Hey", "Bob"),
  eq,
  "Hey Bob",
)

// Optional arguments with Option type
let maybeAdd = (~extra=?, base) =>
  switch extra {
  | Some(e) => base + e
  | None => base
  }
Test.run(__POS_OF__("fn: option arg none"), maybeAdd(10), eq, 10)
Test.run(__POS_OF__("fn: option arg some"), maybeAdd(~extra=5, 10), eq, 15)

// Recursive function
let rec factorial = n =>
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
Test.run(__POS_OF__("fn: recursive"), factorial(5), eq, 120)

// Mutual recursion
let rec isEven = n =>
  if n == 0 {
    true
  } else {
    isOdd(n - 1)
  }
and isOdd = n =>
  if n == 0 {
    false
  } else {
    isEven(n - 1)
  }
Test.run(__POS_OF__("fn: mutual recursion even"), isEven(4), eq, true)
Test.run(__POS_OF__("fn: mutual recursion odd"), isOdd(3), eq, true)

// ─── Pattern matching / switch ──────────────────────────────────────
// Simple switch
let matchInt = x =>
  switch x {
  | 0 => "zero"
  | 1 => "one"
  | _ => "other"
  }
Test.run(__POS_OF__("switch: int match 0"), matchInt(0), eq, "zero")
Test.run(__POS_OF__("switch: int match 1"), matchInt(1), eq, "one")
Test.run(__POS_OF__("switch: int match wildcard"), matchInt(99), eq, "other")

// Switch on string
let matchStr = s =>
  switch s {
  | "hello" => 1
  | "world" => 2
  | "" => 0
  | _ => -1
  }
Test.run(__POS_OF__("switch: string match"), matchStr("hello"), eq, 1)
Test.run(__POS_OF__("switch: string empty"), matchStr(""), eq, 0)
Test.run(__POS_OF__("switch: string wildcard"), matchStr("xyz"), eq, -1)

// Switch on tuple
let matchTuple = ((a, b)) =>
  switch (a, b) {
  | (0, 0) => "origin"
  | (0, _) => "y-axis"
  | (_, 0) => "x-axis"
  | _ => "other"
  }
Test.run(__POS_OF__("switch: tuple origin"), matchTuple((0, 0)), eq, "origin")
Test.run(__POS_OF__("switch: tuple y-axis"), matchTuple((0, 5)), eq, "y-axis")
Test.run(__POS_OF__("switch: tuple x-axis"), matchTuple((3, 0)), eq, "x-axis")
Test.run(__POS_OF__("switch: tuple other"), matchTuple((1, 1)), eq, "other")

// Switch with guard
let classify = x =>
  switch x {
  | n if n < 0 => "negative"
  | 0 => "zero"
  | n if n > 100 => "large"
  | _ => "positive"
  }
Test.run(__POS_OF__("switch: guard negative"), classify(-5), eq, "negative")
Test.run(__POS_OF__("switch: guard zero"), classify(0), eq, "zero")
Test.run(__POS_OF__("switch: guard large"), classify(200), eq, "large")
Test.run(__POS_OF__("switch: guard positive"), classify(50), eq, "positive")

// Switch on option
let showOpt = opt =>
  switch opt {
  | Some(v) => Int.toString(v)
  | None => "none"
  }
Test.run(__POS_OF__("switch: option some"), showOpt(Some(42)), eq, "42")
Test.run(__POS_OF__("switch: option none"), showOpt(None), eq, "none")

// Switch with or-pattern
let isVowel = c =>
  switch c {
  | 'a' | 'e' | 'i' | 'o' | 'u' => true
  | _ => false
  }
Test.run(__POS_OF__("switch: or-pattern match"), isVowel('a'), eq, true)
Test.run(__POS_OF__("switch: or-pattern no match"), isVowel('b'), eq, false)

// Nested pattern matching
type tree = Leaf | Node(tree, int, tree)
let rec treeSum = t =>
  switch t {
  | Leaf => 0
  | Node(l, v, r) => treeSum(l) + v + treeSum(r)
  }
let testTree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))
Test.run(__POS_OF__("switch: nested tree sum"), treeSum(testTree), eq, 6)

// ─── If/else expressions ────────────────────────────────────────────
Test.run(__POS_OF__("if: true branch"), if true { 1 } else { 2 }, eq, 1)
Test.run(__POS_OF__("if: false branch"), if false { 1 } else { 2 }, eq, 2)

// If/else chain
let sign = x =>
  if x > 0 {
    "positive"
  } else if x < 0 {
    "negative"
  } else {
    "zero"
  }
Test.run(__POS_OF__("if: chain positive"), sign(5), eq, "positive")
Test.run(__POS_OF__("if: chain negative"), sign(-3), eq, "negative")
Test.run(__POS_OF__("if: chain zero"), sign(0), eq, "zero")

// ─── Try/catch ──────────────────────────────────────────────────────
exception TestError(string)

let tryCatchResult =
  try {
    raise(TestError("oops"))
  } catch {
  | TestError(msg) => msg
  }
Test.run(__POS_OF__("try: catch exception"), tryCatchResult, eq, "oops")

let tryNoThrow =
  try {
    "ok"
  } catch {
  | _ => "error"
  }
Test.run(__POS_OF__("try: no throw"), tryNoThrow, eq, "ok")

// ─── Block expressions ─────────────────────────────────────────────
let blockResult = {
  let a = 10
  let b = 20
  a + b
}
Test.run(__POS_OF__("block: returns last expr"), blockResult, eq, 30)

// ─── Ternary-like ───────────────────────────────────────────────────
let ternary = true ? "yes" : "no"
Test.run(__POS_OF__("ternary: true"), ternary, eq, "yes")
let ternary2 = false ? "yes" : "no"
Test.run(__POS_OF__("ternary: false"), ternary2, eq, "no")

// ─── Lazy expressions ──────────────────────────────────────────────
let lazyVal = lazy(42)
Test.run(__POS_OF__("lazy: force"), Lazy.force(lazyVal), eq, 42)

let evalCount = ref(0)
let lazySideEffect = lazy({
  evalCount := evalCount.contents + 1
  "computed"
})
let _ = Lazy.force(lazySideEffect)
let _ = Lazy.force(lazySideEffect)
Test.run(__POS_OF__("lazy: memoized"), evalCount.contents, eq, 1)

// ─── Assert expression ─────────────────────────────────────────────
// assert(true) should not throw
let assertOk = try {
  assert(true)
  "ok"
} catch {
| _ => "fail"
}
Test.run(__POS_OF__("assert: true passes"), assertOk, eq, "ok")

// ─── Sequence / semicolons ──────────────────────────────────────────
let seqRef = ref(0)
let seqResult = {
  seqRef := 1
  seqRef := seqRef.contents + 1
  seqRef.contents
}
Test.run(__POS_OF__("sequence: side effects"), seqResult, eq, 2)

// ─── Nested expressions ────────────────────────────────────────────
Test.run(
  __POS_OF__("nested: complex arithmetic"),
  (1 + 2) * (3 + 4) - 5 * (6 - 7),
  eq,
  26,
)

// ─── Array access and update ────────────────────────────────────────
let arr = [10, 20, 30]
Test.run(__POS_OF__("array: get"), arr[1], eq, 20)
let arr2 = arr->Array.copy
arr2[0] = 99
Test.run(__POS_OF__("array: set"), arr2[0], eq, 99)
Test.run(__POS_OF__("array: original unchanged"), arr[0], eq, 10)

// ─── String interpolation as expression ─────────────────────────────
let interpResult = {
  let x = 10
  let y = 20
  `${Int.toString(x)} + ${Int.toString(y)} = ${Int.toString(x + y)}`
}
Test.run(__POS_OF__("interp: expression"), interpResult, eq, "10 + 20 = 30")

// ─── Module access ──────────────────────────────────────────────────
module Inner = {
  let value = 42
  module Nested = {
    let deep = 99
  }
}
Test.run(__POS_OF__("module: simple access"), Inner.value, eq, 42)
Test.run(__POS_OF__("module: nested access"), Inner.Nested.deep, eq, 99)

// ─── First-class module ─────────────────────────────────────────────
module type Showable = {
  type t
  let show: t => string
}

module IntShow = {
  type t = int
  let show = Int.toString
}

let showValue = (type a, module(S: Showable with type t = a), v: a) => S.show(v)
Test.run(__POS_OF__("first-class module"), showValue(module(IntShow), 42), eq, "42")

// ─── Exception expressions ─────────────────────────────────────────
exception NotFound
exception WithPayload(int, string)

let exnTest =
  try {
    raise(WithPayload(404, "not found"))
  } catch {
  | WithPayload(code, msg) => `${Int.toString(code)}: ${msg}`
  | NotFound => "not found"
  | _ => "unknown"
  }
Test.run(__POS_OF__("exception: payload"), exnTest, eq, "404: not found")

// ─── Higher-order functions ─────────────────────────────────────────
let apply = (f, x) => f(x)
let compose = (f, g, x) => f(g(x))
Test.run(__POS_OF__("hof: apply"), apply(x => x + 1, 5), eq, 6)
Test.run(
  __POS_OF__("hof: compose"),
  compose(x => x * 2, x => x + 1, 3),
  eq,
  8,
)
