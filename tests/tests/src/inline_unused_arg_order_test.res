// When a call to a small function is inlined, the arguments it ignores are
// still evaluated for their side effects. They used to be evaluated in the
// iteration order of a hash table instead of parameter order. Arguments the
// function uses are substituted into its body, so they still run afterwards.
let recorded: array<int> = []

let log = x => {
  recorded->Array.push(x)
  x
}

let lastOnly = (_a, _b, _c, d) => Math.Int.max(d, 0)
let applyFirst = (k, a, _b, _c) => k(a)

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("unused arguments before a primitive", () => {
    recorded->Array.splice(~start=0, ~remove=Array.length(recorded), ~insert=[])
    let r = lastOnly(log(1), log(2), log(3), log(4))
    eq(__LOC__, r, 4)
    eq(__LOC__, recorded, [1, 2, 3, 4])
  })

  test("unused arguments before an application", () => {
    recorded->Array.splice(~start=0, ~remove=Array.length(recorded), ~insert=[])
    let r = applyFirst(Math.Int.abs, log(1), log(2), log(3))
    eq(__LOC__, r, 1)
    eq(__LOC__, recorded, [2, 3, 1])
  })
})
