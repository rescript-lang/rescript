open Mocha
open Test_utils

/***
`compare(a, b) == 0`, `< 0` and `> 0` are folded to `a === b`, `a < b` and
`a > b` for any type, not only ints, so structural comparison of records,
tuples, arrays, options and NaN gives the wrong answer.
*/

@inline(never) let id = x => x

type point = {x: int}

describe(__MODULE__, () => {
  test("records", () => {
    let a = id({x: 1})
    let b = id({x: 1})
    let c = id({x: 2})
    // Actual: false, expected: true
    eq(__LOC__, compare(a, b) == 0, true)
    // Actual: false, expected: true
    eq(__LOC__, compare(a, c) < 0, true)
    // Actual: false, expected: true
    eq(__LOC__, compare(c, a) > 0, true)
  })

  test("tuples, arrays and options", () => {
    // Actual: false, expected: true
    eq(__LOC__, compare(id((1, 2)), id((1, 2))) == 0, true)
    // Actual: true, expected: false
    eq(__LOC__, compare(id([10]), id([9])) < 0, false)
    // Actual: false, expected: true
    eq(__LOC__, compare(id(None), id(Some(1))) < 0, true)
  })

  test("NaN", () => {
    let nan = id(Float.Constants.nan)
    // Actual: false, expected: true
    eq(__LOC__, compare(nan, id(1.0)) < 0, true)
    // Actual: false, expected: true
    eq(__LOC__, compare(nan, nan) == 0, true)
  })
})
