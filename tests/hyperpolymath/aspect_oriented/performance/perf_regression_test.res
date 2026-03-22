// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Performance regression
// Cross-cutting concern: performance-sensitive patterns must produce correct
// results even under stress. This validates correctness, not timing.

let eq = (a, b) => a == b

// ─── Large pattern match ────────────────────────────────────────────
type bigEnum =
  | V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
  | V10 | V11 | V12 | V13 | V14 | V15 | V16 | V17 | V18 | V19
  | V20 | V21 | V22 | V23 | V24 | V25 | V26 | V27 | V28 | V29
  | V30 | V31 | V32 | V33 | V34 | V35 | V36 | V37 | V38 | V39
  | V40 | V41 | V42 | V43 | V44 | V45 | V46 | V47 | V48 | V49

let enumToInt = v =>
  switch v {
  | V0 => 0 | V1 => 1 | V2 => 2 | V3 => 3 | V4 => 4
  | V5 => 5 | V6 => 6 | V7 => 7 | V8 => 8 | V9 => 9
  | V10 => 10 | V11 => 11 | V12 => 12 | V13 => 13 | V14 => 14
  | V15 => 15 | V16 => 16 | V17 => 17 | V18 => 18 | V19 => 19
  | V20 => 20 | V21 => 21 | V22 => 22 | V23 => 23 | V24 => 24
  | V25 => 25 | V26 => 26 | V27 => 27 | V28 => 28 | V29 => 29
  | V30 => 30 | V31 => 31 | V32 => 32 | V33 => 33 | V34 => 34
  | V35 => 35 | V36 => 36 | V37 => 37 | V38 => 38 | V39 => 39
  | V40 => 40 | V41 => 41 | V42 => 42 | V43 => 43 | V44 => 44
  | V45 => 45 | V46 => 46 | V47 => 47 | V48 => 48 | V49 => 49
  }

Test.run(__POS_OF__("perf: big enum 0"), enumToInt(V0), eq, 0)
Test.run(__POS_OF__("perf: big enum 25"), enumToInt(V25), eq, 25)
Test.run(__POS_OF__("perf: big enum 49"), enumToInt(V49), eq, 49)

// ─── Deep recursion ────────────────────────────────────────────────
let rec tailSum = (n, acc) =>
  if n <= 0 {
    acc
  } else {
    tailSum(n - 1, acc + n)
  }
Test.run(__POS_OF__("perf: deep recursion"), tailSum(100000, 0), eq, 5000050000)

// ─── Large array operations ─────────────────────────────────────────
let largeArr = Array.fromInitializer(~length=10000, i => i)

let mapResult = largeArr->Array.map(x => x * 2)->Array.reduce(0, (a, b) => a + b)
// Sum of 0..9999 doubled = 2 * (9999 * 10000 / 2) = 99990000
Test.run(__POS_OF__("perf: large map+reduce"), mapResult, eq, 99990000)

let filterResult = largeArr->Array.filter(x => x mod 2 == 0)->Array.length
Test.run(__POS_OF__("perf: large filter"), filterResult, eq, 5000)

let findResult = largeArr->Array.find(x => x == 9999)
Test.run(__POS_OF__("perf: large find"), findResult, eq, Some(9999))

let someResult = largeArr->Array.some(x => x == 9999)
Test.run(__POS_OF__("perf: large some"), someResult, eq, true)

let everyResult = largeArr->Array.every(x => x >= 0)
Test.run(__POS_OF__("perf: large every"), everyResult, eq, true)

// ─── String building ────────────────────────────────────────────────
let buildString = n => {
  let parts = Array.fromInitializer(~length=n, i => Int.toString(i))
  parts->Array.join(",")
}
let built = buildString(1000)
Test.run(__POS_OF__("perf: string build length"), String.length(built) > 2000, eq, true)
Test.run(__POS_OF__("perf: string build start"), String.startsWith(built, "0,1,2"), eq, true)

// ─── Nested module access ──────────────────────────────────────────
module Perf = {
  module Deep = {
    module Chain = {
      module Access = {
        let value = 42
        let compute = x => x * 2 + 1
      }
    }
  }
}

let nestedAccess = Perf.Deep.Chain.Access.compute(Perf.Deep.Chain.Access.value)
Test.run(__POS_OF__("perf: nested module"), nestedAccess, eq, 85)

// ─── Heavy polymorphic variant usage ────────────────────────────────
type event =
  | #click(int, int)
  | #keypress(string)
  | #scroll(float)
  | #resize(int, int)
  | #focus
  | #blur
  | #mouseenter(int, int)
  | #mouseleave
  | #touchstart(array<(int, int)>)
  | #touchend

let handleEvent = e =>
  switch e {
  | #click(x, y) => `click:${Int.toString(x)},${Int.toString(y)}`
  | #keypress(k) => `key:${k}`
  | #scroll(s) => `scroll:${Float.toString(s)}`
  | #resize(w, h) => `resize:${Int.toString(w)}x${Int.toString(h)}`
  | #focus => "focus"
  | #blur => "blur"
  | #mouseenter(x, y) => `enter:${Int.toString(x)},${Int.toString(y)}`
  | #mouseleave => "leave"
  | #touchstart(points) => `touch:${Int.toString(Array.length(points))}`
  | #touchend => "touchend"
  }

Test.run(__POS_OF__("perf: polyvar click"), handleEvent(#click(10, 20)), eq, "click:10,20")
Test.run(__POS_OF__("perf: polyvar key"), handleEvent(#keypress("a")), eq, "key:a")
Test.run(__POS_OF__("perf: polyvar focus"), handleEvent(#focus), eq, "focus")

// ─── Iterator patterns ─────────────────────────────────────────────
let iterResult = {
  let acc = ref(0)
  let arr = Array.fromInitializer(~length=1000, i => i + 1)
  arr->Array.forEach(x => acc := acc.contents + x)
  acc.contents
}
Test.run(__POS_OF__("perf: forEach 1000"), iterResult, eq, 500500)

// ─── Memoization pattern ───────────────────────────────────────────
let memoFib = {
  let cache = Dict.make()
  let rec fib = n => {
    let key = Int.toString(n)
    switch Dict.get(cache, key) {
    | Some(v) => v
    | None =>
      let v = if n <= 1 {
        n
      } else {
        fib(n - 1) + fib(n - 2)
      }
      Dict.set(cache, key, v)
      v
    }
  }
  fib
}
Test.run(__POS_OF__("perf: memoized fib 30"), memoFib(30), eq, 832040)
Test.run(__POS_OF__("perf: memoized fib 40"), memoFib(40), eq, 102334155)

// ─── Lazy evaluation patterns ──────────────────────────────────────
let lazyExpensive = lazy({
  let sum = ref(0)
  for i in 1 to 10000 {
    sum := sum.contents + i
  }
  sum.contents
})
Test.run(__POS_OF__("perf: lazy first"), Lazy.force(lazyExpensive), eq, 50005000)
Test.run(__POS_OF__("perf: lazy cached"), Lazy.force(lazyExpensive), eq, 50005000)

// ─── Complex data pipeline ─────────────────────────────────────────
let pipelineResult =
  Array.fromInitializer(~length=1000, i => i)
  ->Array.filter(x => x mod 3 == 0) // multiples of 3
  ->Array.map(x => x * x)            // square them
  ->Array.filter(x => x mod 2 == 0)  // keep even squares
  ->Array.reduce(0, (a, b) => a + b) // sum

Test.run(__POS_OF__("perf: data pipeline"), pipelineResult > 0, eq, true)

// ─── Closure creation performance ──────────────────────────────────
let closures = Array.fromInitializer(~length=100, i => () => i * i)
let closureResults = closures->Array.map(f => f())
Test.run(__POS_OF__("perf: closures[0]"), closureResults[0], eq, 0)
Test.run(__POS_OF__("perf: closures[50]"), closureResults[50], eq, 2500)
Test.run(__POS_OF__("perf: closures[99]"), closureResults[99], eq, 9801)
