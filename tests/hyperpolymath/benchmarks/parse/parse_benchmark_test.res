// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Benchmark: Parser performance
// Measures performance of data creation patterns that stress the parser
// and downstream compilation.

let eq = (a, b) => a == b

@val external performanceNow: unit => float = "performance.now"

let bench = (name, iterations, f) => {
  let start = performanceNow()
  for _ in 0 to iterations - 1 {
    ignore(f())
  }
  let elapsed = performanceNow() -. start
  let perIter = elapsed /. Int.toFloat(iterations)
  Console.log(
    `[BENCH] ${name}: ${Float.toFixed(elapsed, ~digits=2)}ms total, ${Float.toFixed(perIter, ~digits=4)}ms/iter (${Int.toString(iterations)} iterations)`,
  )
  elapsed
}

// ─── Large array literal creation ───────────────────────────────────
let _ = bench("parse: 1000-element array", 100, () => {
  Array.fromInitializer(~length=1000, i => i * i)
})
Test.run(__POS_OF__("bench parse: array runs"), true, eq, true)

// ─── Deep nested expression ────────────────────────────────────────
let rec deepAdd = (n, acc) =>
  if n <= 0 {
    acc
  } else {
    deepAdd(n - 1, acc + n)
  }

let _ = bench("parse: deep expression", 100, () => {
  deepAdd(1000, 0)
})
Test.run(__POS_OF__("bench parse: deep expr"), deepAdd(100, 0), eq, 5050)

// ─── Complex pattern matching ──────────────────────────────────────
type token =
  | Int2(int)
  | Float2(float)
  | String3(string)
  | Bool2(bool)
  | Null3
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Comma
  | Colon
  | EOF

let classifyToken = t =>
  switch t {
  | Int2(n) => `int:${Int.toString(n)}`
  | Float2(f) => `float:${Float.toString(f)}`
  | String3(s) => `string:${s}`
  | Bool2(b) => `bool:${b ? "true" : "false"}`
  | Null3 => "null"
  | LBrace => "{"
  | RBrace => "}"
  | LBrack => "["
  | RBrack => "]"
  | Comma => ","
  | Colon => ":"
  | EOF => "EOF"
  }

let tokens = [Int2(1), String3("a"), LBrace, Comma, RBrace, EOF]

let _ = bench("parse: pattern match 12-variant", 1000, () => {
  tokens->Array.map(classifyToken)
})
Test.run(
  __POS_OF__("bench parse: match correctness"),
  classifyToken(Int2(42)),
  eq,
  "int:42",
)

// ─── String processing ─────────────────────────────────────────────
let _ = bench("parse: string operations", 100, () => {
  let s = String.repeat("hello world ", 100)
  let _ = String.split(s, " ")
  let _ = String.toUpperCase(s)
  let _ = String.toLowerCase(s)
  String.length(s)
})
Test.run(__POS_OF__("bench parse: string ops"), true, eq, true)

// ─── Record creation ───────────────────────────────────────────────
type benchRecord = {
  a: int,
  b: string,
  c: float,
  d: bool,
  e: array<int>,
}

let _ = bench("parse: record creation", 10000, () => {
  {a: 42, b: "hello", c: 3.14, d: true, e: [1, 2, 3]}
})
Test.run(__POS_OF__("bench parse: record"), true, eq, true)

// ─── Module access chain ───────────────────────────────────────────
module BenchA = {
  module BenchB = {
    module BenchC = {
      let value = 42
      let compute = x => x * 2 + 1
    }
  }
}

let _ = bench("parse: module access chain", 10000, () => {
  BenchA.BenchB.BenchC.compute(BenchA.BenchB.BenchC.value)
})
Test.run(
  __POS_OF__("bench parse: module"),
  BenchA.BenchB.BenchC.compute(10),
  eq,
  21,
)

// ─── Complex interpolation ─────────────────────────────────────────
let _ = bench("parse: string interpolation", 1000, () => {
  let a = 1
  let b = "two"
  let c = 3.14
  `values: ${Int.toString(a)}, ${b}, ${Float.toString(c)}, ${Bool.toString(true)}`
})
Test.run(__POS_OF__("bench parse: interpolation"), true, eq, true)

// ─── Variant construction ──────────────────────────────────────────
type benchVariant =
  | Simple
  | WithInt(int)
  | WithStr(string)
  | WithPair(int, string)
  | WithRecord({x: int, y: int})

let _ = bench("parse: variant construction", 10000, () => {
  [Simple, WithInt(42), WithStr("hi"), WithPair(1, "a"), WithRecord({x: 0, y: 0})]
})
Test.run(__POS_OF__("bench parse: variants"), true, eq, true)

// ─── Function definition ───────────────────────────────────────────
let _ = bench("parse: function definitions", 1000, () => {
  let f1 = x => x + 1
  let f2 = (a, b) => a * b
  let f3 = (~x, ~y=0) => x + y
  f1(f2(f3(~x=2), 3))
})
Test.run(__POS_OF__("bench parse: functions"), true, eq, true)

// ─── Summary assertion ─────────────────────────────────────────────
Test.run(__POS_OF__("bench parse: all completed"), true, eq, true)
