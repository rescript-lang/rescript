// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Output determinism
// Cross-cutting concern: same inputs must always produce same outputs.
// Tests sort stability, evaluation order, hash determinism, and more.

let eq = (a, b) => a == b

// ─── Sort stability ────────────────────────────────────────────────
// Stable sort should preserve order of equal elements
type item = {key: int, index: int}

let items = [
  {key: 3, index: 0},
  {key: 1, index: 1},
  {key: 3, index: 2},
  {key: 2, index: 3},
  {key: 1, index: 4},
  {key: 3, index: 5},
]

let sorted = items->Array.toSorted((a, b) => Float.fromInt(a.key - b.key))

// Elements with same key should maintain relative order
Test.run(
  __POS_OF__("determinism: sort stable key=1 order"),
  sorted->Array.filter(i => i.key == 1)->Array.map(i => i.index),
  eq,
  [1, 4],
)
Test.run(
  __POS_OF__("determinism: sort stable key=3 order"),
  sorted->Array.filter(i => i.key == 3)->Array.map(i => i.index),
  eq,
  [0, 2, 5],
)

// ─── Evaluation order of function arguments ─────────────────────────
// Arguments should be evaluated left to right
let evalOrder = ref([])
let track = (label, value) => {
  ignore(evalOrder.contents->Array.push(label))
  value
}

let add3 = (a, b, c) => a + b + c
let _ = add3(track("a", 1), track("b", 2), track("c", 3))
Test.run(
  __POS_OF__("determinism: arg eval order"),
  evalOrder.contents,
  eq,
  ["a", "b", "c"],
)

// ─── Short-circuit evaluation determinism ───────────────────────────
let scOrder = ref([])

let scTrue = label => {
  ignore(scOrder.contents->Array.push(label))
  true
}
let scFalse = label => {
  ignore(scOrder.contents->Array.push(label))
  false
}

// AND short-circuit: second not evaluated when first is false
scOrder := []
let _ = scFalse("1st") && scTrue("2nd")
Test.run(__POS_OF__("determinism: and short-circuit"), scOrder.contents, eq, ["1st"])

// OR short-circuit: second not evaluated when first is true
scOrder := []
let _ = scTrue("1st") || scFalse("2nd")
Test.run(__POS_OF__("determinism: or short-circuit"), scOrder.contents, eq, ["1st"])

// Full evaluation when needed
scOrder := []
let _ = scTrue("1st") && scTrue("2nd")
Test.run(
  __POS_OF__("determinism: and full eval"),
  scOrder.contents,
  eq,
  ["1st", "2nd"],
)

// ─── Pattern match evaluation order ─────────────────────────────────
// Guards should be evaluated in order
let guardOrder = ref([])
let trackGuard = (label, result) => {
  ignore(guardOrder.contents->Array.push(label))
  result
}

let _ =
  switch 5 {
  | n if trackGuard("g1", n > 10) => "large"
  | n if trackGuard("g2", n > 3) => "medium"
  | _ => "small"
  }

Test.run(
  __POS_OF__("determinism: guard eval order"),
  guardOrder.contents,
  eq,
  ["g1", "g2"],
)

// ─── Module initialization order ────────────────────────────────────
let initOrder = ref([])

module Init1 = {
  let _ = ignore(initOrder.contents->Array.push("Init1"))
  let v = 1
}

module Init2 = {
  let _ = ignore(initOrder.contents->Array.push("Init2"))
  let v = 2
}

module Init3 = {
  let _ = ignore(initOrder.contents->Array.push("Init3"))
  let v = 3
}

Test.run(
  __POS_OF__("determinism: module init order"),
  initOrder.contents,
  eq,
  ["Init1", "Init2", "Init3"],
)
Test.run(
  __POS_OF__("determinism: module values"),
  Init1.v + Init2.v + Init3.v,
  eq,
  6,
)

// ─── Side effect ordering ───────────────────────────────────────────
let sideEffects = ref([])
let sideEffect = (label, value) => {
  ignore(sideEffects.contents->Array.push(label))
  value
}

// Sequential side effects
sideEffects := []
let _ = sideEffect("a", 1)
let _ = sideEffect("b", 2)
let _ = sideEffect("c", 3)
Test.run(
  __POS_OF__("determinism: sequential side effects"),
  sideEffects.contents,
  eq,
  ["a", "b", "c"],
)

// Side effects in array creation
sideEffects := []
let _ = [sideEffect("x", 1), sideEffect("y", 2), sideEffect("z", 3)]
Test.run(
  __POS_OF__("determinism: array creation order"),
  sideEffects.contents,
  eq,
  ["x", "y", "z"],
)

// ─── Hash/comparison determinism ────────────────────────────────────
// Same values should always compare equal
let hash1 = [1, 2, 3]
let hash2 = [1, 2, 3]
Test.run(__POS_OF__("determinism: array eq"), hash1 == hash2, eq, true)

let rec1 = {key: 1, index: 0}
let rec2 = {key: 1, index: 0}
Test.run(__POS_OF__("determinism: record eq"), rec1 == rec2, eq, true)

// ─── Map/Dict iteration order ───────────────────────────────────────
// Dict insertion order should be preserved
let d = Dict.make()
Dict.set(d, "first", 1)
Dict.set(d, "second", 2)
Dict.set(d, "third", 3)
let keys = Dict.keysToArray(d)
Test.run(__POS_OF__("determinism: dict key order"), keys, eq, ["first", "second", "third"])

// ─── Repeated computation determinism ───────────────────────────────
// Same computation should always produce same result
let compute = () => {
  let arr = Array.fromInitializer(~length=100, i => i)
  arr
  ->Array.filter(x => x mod 3 == 0)
  ->Array.map(x => x * x)
  ->Array.reduce(0, (a, b) => a + b)
}

let result1 = compute()
let result2 = compute()
let result3 = compute()
Test.run(__POS_OF__("determinism: repeated 1==2"), result1, eq, result2)
Test.run(__POS_OF__("determinism: repeated 2==3"), result2, eq, result3)

// ─── For loop determinism ──────────────────────────────────────────
let loopResult1 = {
  let acc = ref(0)
  for i in 1 to 100 {
    acc := acc.contents + i
  }
  acc.contents
}
let loopResult2 = {
  let acc = ref(0)
  for i in 1 to 100 {
    acc := acc.contents + i
  }
  acc.contents
}
Test.run(__POS_OF__("determinism: loop"), loopResult1, eq, loopResult2)
Test.run(__POS_OF__("determinism: loop value"), loopResult1, eq, 5050)

// ─── String operations determinism ──────────────────────────────────
let str = "Hello, World!"
Test.run(
  __POS_OF__("determinism: string ops"),
  String.toLowerCase(str) ++ String.toUpperCase(str),
  eq,
  "hello, world!HELLO, WORLD!",
)
