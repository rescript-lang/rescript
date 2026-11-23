open Mocha
open Test_utils

/***
`compare(a, b) <op> 0` is folded to `a <op> b` only where JS operators agree
with `compare` (int, bool, string, bigint); structural comparison of records,
tuples, arrays, options and NaN floats keeps calling the runtime.
*/

@inline(never) let id = x => x

type point = {x: int}

describe(__MODULE__, () => {
  test("records", () => {
    let a = id({x: 1})
    let b = id({x: 1})
    let c = id({x: 2})
    eq(__LOC__, compare(a, b) == 0, true)
    eq(__LOC__, compare(a, c) < 0, true)
    eq(__LOC__, compare(c, a) > 0, true)
  })

  test("tuples, arrays and options", () => {
    eq(__LOC__, compare(id((1, 2)), id((1, 2))) == 0, true)
    eq(__LOC__, compare(id([10]), id([9])) < 0, false)
    eq(__LOC__, compare(id(None), id(Some(1))) < 0, true)
  })

  test("NaN", () => {
    let nan = id(Float.Constants.nan)
    eq(__LOC__, compare(nan, id(1.0)) < 0, true)
    eq(__LOC__, compare(nan, nan) == 0, true)
  })

  test("primitives that fold", () => {
    eq(__LOC__, compare(id(1), id(2)) < 0, true)
    eq(__LOC__, compare(id("b"), id("a")) > 0, true)
    eq(__LOC__, compare(id(true), id(true)) == 0, true)
    eq(__LOC__, compare(id(2n), id(1n)) >= 0, true)
  })
})
