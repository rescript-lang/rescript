// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Lambda IR — optimizations
// Tests that Lambda IR optimization passes produce correct runtime values.
// We verify constant folding, dead code elimination, beta reduction, inlining,
// tail calls, and pattern match optimization.

let eq = (a, b) => a == b

// ─── Constant folding ───────────────────────────────────────────────
// Arithmetic constants should be folded at compile time
Test.run(__POS_OF__("fold: int add"), 1 + 2, eq, 3)
Test.run(__POS_OF__("fold: int sub"), 10 - 3, eq, 7)
Test.run(__POS_OF__("fold: int mul"), 4 * 5, eq, 20)
Test.run(__POS_OF__("fold: int div"), 10 / 2, eq, 5)
Test.run(__POS_OF__("fold: int mod"), 7 mod 3, eq, 1)
Test.run(__POS_OF__("fold: int complex"), (2 + 3) * (4 - 1), eq, 15)
Test.run(__POS_OF__("fold: int nested"), 1 + 2 + 3 + 4 + 5, eq, 15)

// String constant folding
Test.run(__POS_OF__("fold: string concat"), "hello" ++ " " ++ "world", eq, "hello world")
Test.run(__POS_OF__("fold: string empty concat"), "" ++ "a" ++ "" ++ "b", eq, "ab")

// Boolean constant folding
Test.run(__POS_OF__("fold: bool and"), true && true, eq, true)
Test.run(__POS_OF__("fold: bool or"), false || true, eq, true)
Test.run(__POS_OF__("fold: bool not"), !false, eq, true)
Test.run(__POS_OF__("fold: bool complex"), (true && false) || (true && !false), eq, true)

// Comparison folding
Test.run(__POS_OF__("fold: cmp lt"), 1 < 2, eq, true)
Test.run(__POS_OF__("fold: cmp gt"), 2 > 1, eq, true)
Test.run(__POS_OF__("fold: cmp eq"), 1 == 1, eq, true)
Test.run(__POS_OF__("fold: cmp neq"), 1 != 2, eq, true)

// ─── Dead code elimination ──────────────────────────────────────────
// Unused bindings should be eliminated but program still works
let usedValue = {
  let _unused1 = 999
  let _unused2 = "never read"
  let _unused3 = [1, 2, 3]
  let used = 42
  used
}
Test.run(__POS_OF__("dce: unused bindings"), usedValue, eq, 42)

// Dead branches in conditionals
let dceConditional = {
  if true {
    42
  } else {
    // This entire branch is dead code
    999
  }
}
Test.run(__POS_OF__("dce: dead branch"), dceConditional, eq, 42)

// Dead match arms
type ab = A | B
let dceMatch =
  switch A {
  | A => "a"
  | B => "b" // Dead for this specific invocation
  }
Test.run(__POS_OF__("dce: dead match arm"), dceMatch, eq, "a")

// ─── Beta reduction ─────────────────────────────────────────────────
// Immediately applied functions should be reduced
let betaSimple = ((x) => x + 1)(5)
Test.run(__POS_OF__("beta: simple"), betaSimple, eq, 6)

let betaMulti = ((a, b) => a * b)(3, 4)
Test.run(__POS_OF__("beta: multi-arg"), betaMulti, eq, 12)

let betaNested = ((x) => ((y) => x + y)(10))(20)
Test.run(__POS_OF__("beta: nested"), betaNested, eq, 30)

// ─── Eta conversion ─────────────────────────────────────────────────
// Eta-expanded functions should be reduced
let etaTarget = x => x + 1
let etaWrapper = x => etaTarget(x) // Should be reduced to etaTarget
Test.run(__POS_OF__("eta: simple"), etaWrapper(5), eq, 6)

let etaMulti = (a, b) => add(a, b) where {
  let add = (x, y) => x + y
}
Test.run(__POS_OF__("eta: multi-arg"), etaMulti(3, 4), eq, 7)

// ─── Inlining decisions ─────────────────────────────────────────────
// Small functions should be inlined
let tiny = x => x + 1
let inlineChain = tiny(tiny(tiny(tiny(0))))
Test.run(__POS_OF__("inline: chain small fn"), inlineChain, eq, 4)

// Constant function should be inlined
let constFn = () => 42
Test.run(__POS_OF__("inline: constant fn"), constFn(), eq, 42)

// Projection function should be inlined
let fst = ((a, _b)) => a
let snd = ((_a, b)) => b
Test.run(__POS_OF__("inline: projection fst"), fst((1, 2)), eq, 1)
Test.run(__POS_OF__("inline: projection snd"), snd((1, 2)), eq, 2)

// ─── Tail call optimization ────────────────────────────────────────
// Tail-recursive functions should be optimized to loops
let rec tailSum = (n, acc) =>
  if n <= 0 {
    acc
  } else {
    tailSum(n - 1, acc + n)
  }
Test.run(__POS_OF__("tco: tail sum"), tailSum(100, 0), eq, 5050)
Test.run(__POS_OF__("tco: tail sum large"), tailSum(10000, 0), eq, 50005000)

// Tail-recursive list operations
let rec tailLength = (lst, acc) =>
  switch lst {
  | [] => acc
  | [_, ...rest] => tailLength(rest, acc + 1)
  }
Test.run(__POS_OF__("tco: tail length"), tailLength([1, 2, 3, 4, 5], 0), eq, 5)

// Tail-recursive with pattern matching
let rec tailFib = (n, a, b) =>
  if n <= 0 {
    a
  } else {
    tailFib(n - 1, b, a + b)
  }
Test.run(__POS_OF__("tco: tail fib"), tailFib(10, 0, 1), eq, 55)
Test.run(__POS_OF__("tco: tail fib large"), tailFib(30, 0, 1), eq, 832040)

// ─── Pattern match optimization ─────────────────────────────────────
// Sequential tag variants should compile to efficient switch
type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let dayNumber = d =>
  switch d {
  | Mon => 1
  | Tue => 2
  | Wed => 3
  | Thu => 4
  | Fri => 5
  | Sat => 6
  | Sun => 7
  }

Test.run(__POS_OF__("match opt: sequential mon"), dayNumber(Mon), eq, 1)
Test.run(__POS_OF__("match opt: sequential thu"), dayNumber(Thu), eq, 4)
Test.run(__POS_OF__("match opt: sequential sun"), dayNumber(Sun), eq, 7)

// Or-patterns should be combined
let isWeekend = d =>
  switch d {
  | Sat | Sun => true
  | Mon | Tue | Wed | Thu | Fri => false
  }
Test.run(__POS_OF__("match opt: or-pattern true"), isWeekend(Sat), eq, true)
Test.run(__POS_OF__("match opt: or-pattern false"), isWeekend(Wed), eq, false)

// Nested variant match
type optInt = SomeInt(int) | NoneInt
let unwrapOr = (v, default) =>
  switch v {
  | SomeInt(x) => x
  | NoneInt => default
  }
Test.run(__POS_OF__("match opt: nested some"), unwrapOr(SomeInt(42), 0), eq, 42)
Test.run(__POS_OF__("match opt: nested none"), unwrapOr(NoneInt, 99), eq, 99)

// ─── Let binding optimization ───────────────────────────────────────
// Simple let bindings should be eliminated via alias removal
let letOpt = {
  let x = 10
  let y = x
  let z = y
  z + 1
}
Test.run(__POS_OF__("let opt: alias chain"), letOpt, eq, 11)

// ─── Constant propagation through modules ───────────────────────────
module Constants = {
  let pi = 3.14159
  let e = 2.71828
  let goldenRatio = 1.61803
}

let circleArea = r => Constants.pi *. r *. r
Test.run(__POS_OF__("const prop: module pi"), circleArea(1.0) > 3.14, eq, true)
Test.run(__POS_OF__("const prop: module pi precise"), circleArea(1.0) < 3.15, eq, true)

// ─── Reference elimination ──────────────────────────────────────────
// Simple refs used only locally should be eliminated
let refElim = {
  let r = ref(0)
  r := 42
  r.contents
}
Test.run(__POS_OF__("ref elim: simple"), refElim, eq, 42)

// ─── Deep flatten optimization ──────────────────────────────────────
// Nested let blocks should be flattened
let deepFlatten = {
  let a = {
    let b = {
      let c = 1 + 2
      c * 3
    }
    b + 4
  }
  a + 5
}
Test.run(__POS_OF__("flatten: deep lets"), deepFlatten, eq, 18)

// ─── Closure optimization ───────────────────────────────────────────
// Simple closures should be optimized
let makeAdder = n => x => x + n
let add5 = makeAdder(5)
let add10 = makeAdder(10)
Test.run(__POS_OF__("closure: make adder 5"), add5(3), eq, 8)
Test.run(__POS_OF__("closure: make adder 10"), add10(3), eq, 13)

// Closure captures should be correct
let closureCapture = {
  let results = []
  for i in 0 to 4 {
    let captured = i
    ignore(results->Array.push(() => captured))
  }
  results->Array.map(f => f())
}
Test.run(__POS_OF__("closure: loop capture"), closureCapture, eq, [0, 1, 2, 3, 4])

// ─── Exit code optimization ────────────────────────────────────────
// Switch with shared exit codes
type shape2 = Circle2(float) | Rect2(float, float) | Square2(float) | Dot2
let isFlat = s =>
  switch s {
  | Dot2 => true
  | Circle2(_) | Square2(_) | Rect2(_, _) => false
  }
Test.run(__POS_OF__("exit: shared code true"), isFlat(Dot2), eq, true)
Test.run(__POS_OF__("exit: shared code false"), isFlat(Circle2(1.0)), eq, false)
