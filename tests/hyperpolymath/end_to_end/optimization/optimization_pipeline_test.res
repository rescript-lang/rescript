// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// End-to-end test: Optimization pipeline
// Tests that the full optimization pipeline (constant folding, DCE, inlining,
// tail calls, pattern match optimization) produces correct runtime values.

let eq = (a, b) => a == b

// ─── Constant folding through pipeline ──────────────────────────────
// These should all be folded at compile time
Test.run(__POS_OF__("opt fold: arithmetic"), 2 * 3 + 4 * 5 - 1, eq, 25)
Test.run(__POS_OF__("opt fold: string"), "a" ++ "b" ++ "c" ++ "d", eq, "abcd")
Test.run(__POS_OF__("opt fold: bool chain"), true && (false || true) && !false, eq, true)
Test.run(__POS_OF__("opt fold: comparison"), (1 < 2) && (3 >= 3) && (4 != 5), eq, true)

// Constant folding across let bindings
let constA = 10
let constB = 20
let constC = constA + constB
Test.run(__POS_OF__("opt fold: across lets"), constC, eq, 30)

// ─── Dead code elimination end-to-end ───────────────────────────────
let dceResult = {
  let _dead1 = Array.fromInitializer(~length=100, i => i)
  let _dead2 = "this string is never used"
  let _dead3 = (a, b) => a + b
  42
}
Test.run(__POS_OF__("opt dce: unused bindings"), dceResult, eq, 42)

// Dead branch elimination
let dceBranch = x =>
  if true {
    x + 1
  } else {
    x * 1000 // dead
  }
Test.run(__POS_OF__("opt dce: dead branch"), dceBranch(5), eq, 6)

// ─── Function inlining ─────────────────────────────────────────────
// Small functions should be inlined
let incr = x => x + 1
let decr = x => x - 1
let double2 = x => x * 2

// Chain of small functions — all should inline
let result = double2(incr(incr(decr(10))))
Test.run(__POS_OF__("opt inline: chain"), result, eq, 22)

// Inline identity
let ident = x => x
Test.run(__POS_OF__("opt inline: identity"), ident(42), eq, 42)

// Inline constant function
let always42 = () => 42
Test.run(__POS_OF__("opt inline: const fn"), always42(), eq, 42)

// ─── Tail call optimization ────────────────────────────────────────
// These recursive functions should be optimized to loops
let rec sumTo = (n, acc) =>
  if n <= 0 {
    acc
  } else {
    sumTo(n - 1, acc + n)
  }
Test.run(__POS_OF__("opt tco: sumTo"), sumTo(10000, 0), eq, 50005000)

// Tail-recursive with pattern matching
let rec countDown = (n, acc) =>
  switch n {
  | 0 => acc
  | n => countDown(n - 1, [n, ...acc])
  }
Test.run(__POS_OF__("opt tco: countDown"), List.toArray(countDown(5, list{})), eq, [1, 2, 3, 4, 5])

// Tail-recursive GCD
let rec gcd = (a, b) =>
  if b == 0 {
    a
  } else {
    gcd(b, a mod b)
  }
Test.run(__POS_OF__("opt tco: gcd"), gcd(48, 18), eq, 6)
Test.run(__POS_OF__("opt tco: gcd coprime"), gcd(17, 13), eq, 1)

// ─── Pattern match optimization ─────────────────────────────────────
// Sequential tags should compile to efficient switch
type month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec

let daysInMonth = (m, ~leapYear=false) =>
  switch m {
  | Jan | Mar | May | Jul | Aug | Oct | Dec => 31
  | Apr | Jun | Sep | Nov => 30
  | Feb => leapYear ? 29 : 28
  }

Test.run(__POS_OF__("opt match: jan"), daysInMonth(Jan), eq, 31)
Test.run(__POS_OF__("opt match: apr"), daysInMonth(Apr), eq, 30)
Test.run(__POS_OF__("opt match: feb"), daysInMonth(Feb), eq, 28)
Test.run(__POS_OF__("opt match: feb leap"), daysInMonth(Feb, ~leapYear=true), eq, 29)

// Or-pattern grouping
let isWeekday = d =>
  switch d {
  | #mon | #tue | #wed | #thu | #fri => true
  | #sat | #sun => false
  }
Test.run(__POS_OF__("opt match: weekday"), isWeekday(#mon), eq, true)
Test.run(__POS_OF__("opt match: weekend"), isWeekday(#sat), eq, false)

// ─── Curry optimization ────────────────────────────────────────────
// Uncurried functions should be called directly
let addUncurried = (a, b) => a + b
Test.run(__POS_OF__("opt curry: direct"), addUncurried(3, 4), eq, 7)

// Partial application should work correctly
let add10 = addUncurried(10, ...)
Test.run(__POS_OF__("opt curry: partial"), add10(5), eq, 15)

// ─── Module access optimization ────────────────────────────────────
module Const = {
  let a = 1
  let b = 2
  let c = 3
}

// Direct module access should be optimized
let moduleSum = Const.a + Const.b + Const.c
Test.run(__POS_OF__("opt module: direct access"), moduleSum, eq, 6)

// ─── Let binding elimination ────────────────────────────────────────
// Trivial let bindings should be eliminated
let letElim = {
  let x = 42
  let y = x
  let z = y
  z
}
Test.run(__POS_OF__("opt let: alias elim"), letElim, eq, 42)

// ─── String operations optimization ─────────────────────────────────
// String concatenation should be optimized
let strOpt = "hello" ++ " " ++ "world" ++ "!"
Test.run(__POS_OF__("opt str: concat"), strOpt, eq, "hello world!")

// ─── Array literal optimization ─────────────────────────────────────
// Array literals should be created efficiently
let arrOpt = [1, 2, 3, 4, 5]
Test.run(__POS_OF__("opt arr: literal"), arrOpt, eq, [1, 2, 3, 4, 5])

// ─── Record creation optimization ──────────────────────────────────
type point = {x: int, y: int}
let ptOpt = {x: 10, y: 20}
Test.run(__POS_OF__("opt record: creation"), ptOpt.x + ptOpt.y, eq, 30)

// ─── Variant creation optimization ──────────────────────────────────
type opt<'a> = None2 | Some2('a)
let optVal = Some2(42)
Test.run(
  __POS_OF__("opt variant: creation"),
  switch optVal {
  | Some2(v) => v
  | None2 => 0
  },
  eq,
  42,
)

// ─── Complex optimization chain ─────────────────────────────────────
// This exercises multiple optimization passes together
let complexOpt = {
  let data = Array.fromInitializer(~length=20, i => i + 1)
  let filtered = data->Array.filter(x => x mod 2 == 0)
  let mapped = filtered->Array.map(x => x * x)
  let sum = mapped->Array.reduce(0, (a, b) => a + b)
  sum
}
// Even numbers 2..20: 2,4,6,8,10,12,14,16,18,20
// Squared: 4,16,36,64,100,144,196,256,324,400
// Sum: 1540
Test.run(__POS_OF__("opt complex: pipeline"), complexOpt, eq, 1540)

// ─── Nested function optimization ───────────────────────────────────
let makeMultiplier = factor => {
  let multiply = x => x * factor
  multiply
}
let triple = makeMultiplier(3)
let quadruple = makeMultiplier(4)
Test.run(__POS_OF__("opt nested fn: triple"), triple(5), eq, 15)
Test.run(__POS_OF__("opt nested fn: quad"), quadruple(5), eq, 20)

// ─── Optimization preserves side effects ────────────────────────────
let sideRef = ref(0)
let withSideEffect = () => {
  sideRef := sideRef.contents + 1
  sideRef.contents
}
let se1 = withSideEffect()
let se2 = withSideEffect()
let se3 = withSideEffect()
Test.run(__POS_OF__("opt side-effect: preserved 1"), se1, eq, 1)
Test.run(__POS_OF__("opt side-effect: preserved 2"), se2, eq, 2)
Test.run(__POS_OF__("opt side-effect: preserved 3"), se3, eq, 3)
