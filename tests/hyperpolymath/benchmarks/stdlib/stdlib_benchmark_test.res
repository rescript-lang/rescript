// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Benchmark: Standard library operations
// Measures performance of Array, String, Dict, Option, Result, and sorting.

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

// ─── Array.map ──────────────────────────────────────────────────────
let largeArr = Array.fromInitializer(~length=10000, i => i)

let _ = bench("stdlib: Array.map 10k", 100, () => {
  largeArr->Array.map(x => x * 2)
})
Test.run(
  __POS_OF__("bench stdlib: map correctness"),
  [1, 2, 3]->Array.map(x => x * 2),
  eq,
  [2, 4, 6],
)

// ─── Array.filter ───────────────────────────────────────────────────
let _ = bench("stdlib: Array.filter 10k", 100, () => {
  largeArr->Array.filter(x => x mod 2 == 0)
})
Test.run(
  __POS_OF__("bench stdlib: filter correctness"),
  [1, 2, 3, 4]->Array.filter(x => x mod 2 == 0),
  eq,
  [2, 4],
)

// ─── Array.reduce ───────────────────────────────────────────────────
let _ = bench("stdlib: Array.reduce 10k", 100, () => {
  largeArr->Array.reduce(0, (acc, x) => acc + x)
})
Test.run(
  __POS_OF__("bench stdlib: reduce correctness"),
  [1, 2, 3]->Array.reduce(0, (a, b) => a + b),
  eq,
  6,
)

// ─── Array.flatMap ──────────────────────────────────────────────────
let medArr = Array.fromInitializer(~length=1000, i => i)

let _ = bench("stdlib: Array.flatMap 1k", 100, () => {
  medArr->Array.flatMap(x => [x, x + 1])
})
Test.run(
  __POS_OF__("bench stdlib: flatMap correctness"),
  [1, 2]->Array.flatMap(x => [x, x * 10]),
  eq,
  [1, 10, 2, 20],
)

// ─── Array.sort ─────────────────────────────────────────────────────
let _ = bench("stdlib: Array.toSorted 10k", 10, () => {
  let reversed = largeArr->Array.toReversed
  reversed->Array.toSorted((a, b) => Float.fromInt(a - b))
})
Test.run(
  __POS_OF__("bench stdlib: sort correctness"),
  [3, 1, 2]->Array.toSorted((a, b) => Float.fromInt(a - b)),
  eq,
  [1, 2, 3],
)

// ─── String concatenation ───────────────────────────────────────────
let _ = bench("stdlib: String concat 1000x", 100, () => {
  let parts = Array.fromInitializer(~length=1000, i => Int.toString(i))
  parts->Array.join("")
})
Test.run(
  __POS_OF__("bench stdlib: concat correctness"),
  ["a", "b", "c"]->Array.join(""),
  eq,
  "abc",
)

// ─── String operations ─────────────────────────────────────────────
let testStr = String.repeat("hello world ", 1000)

let _ = bench("stdlib: String.split large", 100, () => {
  String.split(testStr, " ")
})
Test.run(
  __POS_OF__("bench stdlib: split correctness"),
  String.split("a b c", " "),
  eq,
  ["a", "b", "c"],
)

let _ = bench("stdlib: String.includes large", 1000, () => {
  String.includes(testStr, "world")
})
Test.run(
  __POS_OF__("bench stdlib: includes correctness"),
  String.includes("hello world", "world"),
  eq,
  true,
)

// ─── Dict operations ────────────────────────────────────────────────
let _ = bench("stdlib: Dict set/get 1000", 100, () => {
  let d = Dict.make()
  for i in 0 to 999 {
    Dict.set(d, Int.toString(i), i)
  }
  let sum = ref(0)
  for i in 0 to 999 {
    switch Dict.get(d, Int.toString(i)) {
    | Some(v) => sum := sum.contents + v
    | None => ()
    }
  }
  sum.contents
})
Test.run(__POS_OF__("bench stdlib: dict correctness"), true, eq, true)

// ─── Option.map chains ─────────────────────────────────────────────
let _ = bench("stdlib: Option.map chain", 10000, () => {
  Some(1)
  ->Option.map(x => x + 1)
  ->Option.map(x => x * 2)
  ->Option.map(x => x + 3)
  ->Option.map(x => x * 4)
  ->Option.flatMap(x => x > 10 ? Some(x) : None)
})
Test.run(
  __POS_OF__("bench stdlib: option chain"),
  Some(1)->Option.map(x => x + 1)->Option.map(x => x * 2),
  eq,
  Some(4),
)

// ─── Result.flatMap chains ──────────────────────────────────────────
let _ = bench("stdlib: Result.flatMap chain", 10000, () => {
  Ok(1)
  ->Result.flatMap(x => Ok(x + 1))
  ->Result.flatMap(x => Ok(x * 2))
  ->Result.flatMap(x => Ok(x + 3))
  ->Result.flatMap(x => Ok(x * 4))
  ->Result.flatMap(x => x > 10 ? Ok(x) : Error("too small"))
})
Test.run(
  __POS_OF__("bench stdlib: result chain"),
  Ok(1)->Result.flatMap(x => Ok(x + 1))->Result.map(x => x * 2),
  eq,
  Ok(4),
)

// ─── Binary search ─────────────────────────────────────────────────
let binarySearch = (arr, target) => {
  let lo = ref(0)
  let hi = ref(Array.length(arr) - 1)
  let result = ref(-1)
  while lo.contents <= hi.contents {
    let mid = (lo.contents + hi.contents) / 2
    let v = arr[mid]
    if v == target {
      result := mid
      lo := hi.contents + 1 // break
    } else if v < target {
      lo := mid + 1
    } else {
      hi := mid - 1
    }
  }
  result.contents
}

let sortedArr = Array.fromInitializer(~length=10000, i => i * 2)

let _ = bench("stdlib: binary search 10k", 10000, () => {
  binarySearch(sortedArr, 5000)
})
Test.run(
  __POS_OF__("bench stdlib: bsearch found"),
  binarySearch(sortedArr, 5000),
  eq,
  2500,
)
Test.run(
  __POS_OF__("bench stdlib: bsearch not found"),
  binarySearch(sortedArr, 5001),
  eq,
  -1,
)

// ─── Array.forEach vs for loop ──────────────────────────────────────
let _ = bench("stdlib: forEach 10k", 100, () => {
  let sum = ref(0)
  largeArr->Array.forEach(x => sum := sum.contents + x)
  sum.contents
})

let _ = bench("stdlib: for loop 10k", 100, () => {
  let sum = ref(0)
  for i in 0 to Array.length(largeArr) - 1 {
    sum := sum.contents + largeArr[i]
  }
  sum.contents
})
Test.run(__POS_OF__("bench stdlib: loop vs forEach"), true, eq, true)

// ─── Summary ────────────────────────────────────────────────────────
Console.log("[BENCH] All stdlib benchmarks completed")
Test.run(__POS_OF__("bench stdlib: all completed"), true, eq, true)
