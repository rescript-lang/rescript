open Mocha
open Test_utils

// `e === null || e === undefined` is printed as `e == null` only when `e` has
// no side effects, since `e == null` evaluates `e` once.

let values: array<Nullable.t<int>> = [Nullable.make(1), Nullable.undefined]
let index = ref(0)

let next = () => {
  let v = values->Array.getUnsafe(index.contents)
  index := index.contents + 1
  v
}

type r = {v: Nullable.t<int>}

// Hides values from the optimizer, so the checks below are not folded away
let opaque: 'a => 'a = %raw(`x => x`)

describe(__MODULE__, () => {
  test("operand with side effects", () => {
    index := 0
    eq(__LOC__, next() === Nullable.null || next() === Nullable.undefined, true)
    eq(__LOC__, index.contents, 2)
  })

  test("operand without side effects", () => {
    let r = opaque({v: Nullable.null})
    eq(__LOC__, r.v === Nullable.null || r.v === Nullable.undefined, true)
  })
})
