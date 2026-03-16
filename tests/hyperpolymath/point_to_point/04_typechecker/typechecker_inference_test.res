// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Type Checker — type inference
// Tests that the type checker correctly infers types and produces correct
// runtime behavior across a variety of type inference scenarios.

let eq = (a, b) => a == b

// ─── Basic inference ────────────────────────────────────────────────
// int inference
let inferredInt = 42
Test.run(__POS_OF__("infer: int"), inferredInt, eq, 42)

// float inference
let inferredFloat = 3.14
Test.run(__POS_OF__("infer: float"), inferredFloat > 3.0, eq, true)

// string inference
let inferredStr = "hello"
Test.run(__POS_OF__("infer: string"), inferredStr, eq, "hello")

// bool inference
let inferredBool = true
Test.run(__POS_OF__("infer: bool"), inferredBool, eq, true)

// unit inference
let inferredUnit = ()
Test.run(__POS_OF__("infer: unit"), inferredUnit, eq, ())

// ─── Function type inference ────────────────────────────────────────
// Infer from usage
let addInferred = (a, b) => a + b // int => int => int
Test.run(__POS_OF__("infer: fn int add"), addInferred(3, 4), eq, 7)

let concatInferred = (a, b) => a ++ b // string => string => string
Test.run(__POS_OF__("infer: fn string concat"), concatInferred("a", "b"), eq, "ab")

// Infer return type from body
let makeGreeting = name => `Hello, ${name}!`
Test.run(__POS_OF__("infer: fn return string"), makeGreeting("World"), eq, "Hello, World!")

// Infer from operators
let isPositive = x => x > 0
Test.run(__POS_OF__("infer: fn bool return"), isPositive(5), eq, true)
Test.run(__POS_OF__("infer: fn bool return false"), isPositive(-1), eq, false)

// ─── Polymorphic inference ──────────────────────────────────────────
// Identity function
let id = x => x
Test.run(__POS_OF__("infer: poly identity int"), id(42), eq, 42)
Test.run(__POS_OF__("infer: poly identity str"), id("hi"), eq, "hi")
Test.run(__POS_OF__("infer: poly identity bool"), id(true), eq, true)

// Pair creation
let makePair = (a, b) => (a, b)
let (p1, p2) = makePair(1, "one")
Test.run(__POS_OF__("infer: poly pair fst"), p1, eq, 1)
Test.run(__POS_OF__("infer: poly pair snd"), p2, eq, "one")

// Polymorphic function on collections
let first = arr =>
  switch arr[0] {
  | v => Some(v)
  | exception _ => None
  }
Test.run(__POS_OF__("infer: poly first int"), first([1, 2, 3]), eq, Some(1))
Test.run(__POS_OF__("infer: poly first str"), first(["a", "b"]), eq, Some("a"))

// ─── Record type inference ──────────────────────────────────────────
type point2d = {px: int, py: int}
type point3d = {px3: int, py3: int, pz3: int}

// Infer record type from field access
let getX = p => p.px
Test.run(__POS_OF__("infer: record field"), getX({px: 10, py: 20}), eq, 10)

let origin3d = {px3: 0, py3: 0, pz3: 0}
Test.run(__POS_OF__("infer: record 3d z"), origin3d.pz3, eq, 0)

// Record spread inference
let moveRight = p => {...p, px: p.px + 1}
let moved = moveRight({px: 5, py: 10})
Test.run(__POS_OF__("infer: record spread"), moved.px, eq, 6)
Test.run(__POS_OF__("infer: record spread preserved"), moved.py, eq, 10)

// ─── Variant type inference ─────────────────────────────────────────
type direction = North | South | East | West

let opposite = d =>
  switch d {
  | North => South
  | South => North
  | East => West
  | West => East
  }
Test.run(__POS_OF__("infer: variant opposite"), opposite(North), eq, South)
Test.run(__POS_OF__("infer: variant opposite2"), opposite(East), eq, West)

// Variant with payload inference
type numOrStr = Num(int) | Str(string)
let toString = v =>
  switch v {
  | Num(n) => Int.toString(n)
  | Str(s) => s
  }
Test.run(__POS_OF__("infer: variant payload num"), toString(Num(42)), eq, "42")
Test.run(__POS_OF__("infer: variant payload str"), toString(Str("hi")), eq, "hi")

// ─── Constraint propagation ─────────────────────────────────────────
// Type propagates through let bindings
let constrainedFn = () => {
  let x = 1
  let y = x + 2
  let z = y * 3
  z
}
Test.run(__POS_OF__("infer: constraint propagation"), constrainedFn(), eq, 9)

// Type propagates through conditionals
let conditional = flag =>
  if flag {
    42
  } else {
    0
  }
Test.run(__POS_OF__("infer: constraint if"), conditional(true), eq, 42)
Test.run(__POS_OF__("infer: constraint else"), conditional(false), eq, 0)

// ─── Option/Result inference ────────────────────────────────────────
let safeDivide = (a, b) =>
  if b == 0 {
    None
  } else {
    Some(a / b)
  }
Test.run(__POS_OF__("infer: option some"), safeDivide(10, 2), eq, Some(5))
Test.run(__POS_OF__("infer: option none"), safeDivide(10, 0), eq, None)

let parseNumber = s =>
  switch Int.fromString(s) {
  | Some(n) => Ok(n)
  | None => Error(`Invalid number: ${s}`)
  }
Test.run(__POS_OF__("infer: result ok"), parseNumber("42"), eq, Ok(42))
Test.run(
  __POS_OF__("infer: result error"),
  parseNumber("abc"),
  eq,
  Error("Invalid number: abc"),
)

// ─── Higher-order function inference ────────────────────────────────
let applyTwice = (f, x) => f(f(x))
Test.run(__POS_OF__("infer: hof apply twice"), applyTwice(x => x + 1, 0), eq, 2)
Test.run(
  __POS_OF__("infer: hof apply twice str"),
  applyTwice(s => s ++ "!", "hi"),
  eq,
  "hi!!",
)

let compose = (f, g) => x => f(g(x))
let addOne = x => x + 1
let double = x => x * 2
let addOneThenDouble = compose(double, addOne)
Test.run(__POS_OF__("infer: hof compose"), addOneThenDouble(3), eq, 8)

// ─── Module type inference ──────────────────────────────────────────
module Counter = {
  type t = {mutable value: int}
  let make = () => {value: 0}
  let increment = c => c.value = c.value + 1
  let get = c => c.value
}

let counter = Counter.make()
Counter.increment(counter)
Counter.increment(counter)
Counter.increment(counter)
Test.run(__POS_OF__("infer: module type"), Counter.get(counter), eq, 3)

// ─── Recursive type inference ───────────────────────────────────────
type rec json =
  | Null
  | Bool(bool)
  | Number(float)
  | Str(string)
  | Array(array<json>)
  | Object(array<(string, json)>)

let rec jsonToString = j =>
  switch j {
  | Null => "null"
  | Bool(b) => b ? "true" : "false"
  | Number(n) => Float.toString(n)
  | Str(s) => `"${s}"`
  | Array(arr) =>
    "[" ++ arr->Array.map(jsonToString)->Array.join(", ") ++ "]"
  | Object(pairs) =>
    "{" ++
    pairs->Array.map(((k, v)) => `"${k}": ${jsonToString(v)}`)->Array.join(", ") ++
    "}"
  }

Test.run(__POS_OF__("infer: recursive null"), jsonToString(Null), eq, "null")
Test.run(__POS_OF__("infer: recursive bool"), jsonToString(Bool(true)), eq, "true")
Test.run(
  __POS_OF__("infer: recursive array"),
  jsonToString(Array([Number(1.0), Number(2.0)])),
  eq,
  "[1, 2]",
)
Test.run(
  __POS_OF__("infer: recursive object"),
  jsonToString(Object([("name", Str("test"))])),
  eq,
  `{"name": "test"}`,
)

// ─── Functor type inference ─────────────────────────────────────────
module type Mappable = {
  type t<'a>
  let map: (t<'a>, 'a => 'b) => t<'b>
}

module ArrayMappable: Mappable with type t<'a> = array<'a> = {
  type t<'a> = array<'a>
  let map = Array.map
}

let doubled = ArrayMappable.map([1, 2, 3], x => x * 2)
Test.run(__POS_OF__("infer: functor map"), doubled, eq, [2, 4, 6])

// ─── Let binding patterns with type inference ───────────────────────
let (a1, b1, c1) = (1, "two", true)
Test.run(__POS_OF__("infer: tuple destr int"), a1, eq, 1)
Test.run(__POS_OF__("infer: tuple destr str"), b1, eq, "two")
Test.run(__POS_OF__("infer: tuple destr bool"), c1, eq, true)

// ─── Labeled argument inference ─────────────────────────────────────
let range = (~start, ~end_, ~step=1) => {
  let result = []
  let i = ref(start)
  while i.contents < end_ {
    ignore(result->Array.push(i.contents))
    i := i.contents + step
  }
  result
}
Test.run(__POS_OF__("infer: labeled range"), range(~start=0, ~end_=5), eq, [0, 1, 2, 3, 4])
Test.run(
  __POS_OF__("infer: labeled range step"),
  range(~start=0, ~end_=10, ~step=3),
  eq,
  [0, 3, 6, 9],
)

// ─── Polymorphic variant inference ──────────────────────────────────
let describeValue = v =>
  switch v {
  | #int(n) => `int: ${Int.toString(n)}`
  | #string(s) => `string: ${s}`
  | #bool(b) => `bool: ${b ? "true" : "false"}`
  }

Test.run(__POS_OF__("infer: polyvar int"), describeValue(#int(42)), eq, "int: 42")
Test.run(__POS_OF__("infer: polyvar str"), describeValue(#string("hi")), eq, "string: hi")
Test.run(__POS_OF__("infer: polyvar bool"), describeValue(#bool(true)), eq, "bool: true")
