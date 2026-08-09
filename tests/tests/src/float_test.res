open Mocha
open Test_utils

let float_compare = (x: float, y) => Pervasives.compare(x, y)
let generic_compare = Pervasives.compare
let float_equal = (x: float, y) => x == y
let generic_equal = (a, b) => a == b
let float_notequal = (x: float, y) => x != y
let generic_notequal = (a, b) => a != b
let float_lessthan = (x: float, y) => x < y
let generic_lessthan = (a, b) => a < b
let float_greaterthan = (x: float, y) => x > y
let generic_greaterthan = (a, b) => a > b
let float_lessequal = (x: float, y) => x <= y
let generic_lessequal = (a, b) => a <= b
let float_greaterequal = (x: float, y) => x >= y
let generic_greaterequal = (a, b) => a >= b
let nan = Float.Constants.nan

describe(__MODULE__, () => {
  test("float_test_1", () => {
    eq(__LOC__, classify_float(3.), FP_normal)
    eq(
      __LOC__,
      [-1, 1, 1],
      [(1., 3.), (2., 1.), (3., 2.)]
      ->Array.map(((x, y)) => float_compare(x, y))
      ->Array.map(
        x =>
          if x > 0 {
            1
          } else if x < 0 {
            -1
          } else {
            0
          },
      ),
    )
    eq(__LOC__, log10(10.), 1.)
    eq(__LOC__, Float.fromString("3.0"), Some(3.0))
    eq(__LOC__, float_compare(nan, nan), 0)
    eq(__LOC__, generic_compare(nan, nan), 0)
    eq(__LOC__, float_compare(nan, neg_infinity), -1)
    eq(__LOC__, generic_compare(nan, neg_infinity), -1)
    eq(__LOC__, float_compare(neg_infinity, nan), 1)
    eq(__LOC__, generic_compare(neg_infinity, nan), 1)
    eq(__LOC__, float_equal(nan, nan), false)
    eq(__LOC__, generic_equal(nan, nan), false)
    eq(__LOC__, float_equal(4.2, nan), false)
    eq(__LOC__, generic_equal(4.2, nan), false)
    eq(__LOC__, float_equal(nan, 4.2), false)
    eq(__LOC__, generic_equal(nan, 4.2), false)
    eq(__LOC__, float_notequal(nan, nan), true)
    eq(__LOC__, generic_notequal(nan, nan), true)
    eq(__LOC__, float_notequal(4.2, nan), true)
    eq(__LOC__, generic_notequal(4.2, nan), true)
    eq(__LOC__, float_notequal(nan, 4.2), true)
    eq(__LOC__, generic_notequal(nan, 4.2), true)
    eq(__LOC__, float_lessthan(nan, nan), false)
    eq(__LOC__, generic_lessthan(nan, nan), false)
    eq(__LOC__, float_lessthan(4.2, nan), false)
    eq(__LOC__, generic_lessthan(4.2, nan), false)
    eq(__LOC__, float_lessthan(nan, 4.2), false)
    eq(__LOC__, generic_lessthan(nan, 4.2), false)
    eq(__LOC__, float_greaterthan(nan, nan), false)
    eq(__LOC__, generic_greaterthan(nan, nan), false)
    eq(__LOC__, float_greaterthan(4.2, nan), false)
    eq(__LOC__, generic_greaterthan(4.2, nan), false)
    eq(__LOC__, float_greaterthan(nan, 4.2), false)
    eq(__LOC__, generic_greaterthan(nan, 4.2), false)
    eq(__LOC__, float_lessequal(nan, nan), false)
    eq(__LOC__, generic_lessequal(nan, nan), false)
    eq(__LOC__, generic_lessequal(4.2, nan), false)
    eq(__LOC__, generic_lessequal(4.2, nan), false)
    eq(__LOC__, generic_lessequal(nan, 4.2), false)
    eq(__LOC__, generic_lessequal(nan, 4.2), false)
    eq(__LOC__, float_greaterequal(nan, nan), false)
    eq(__LOC__, generic_greaterequal(nan, nan), false)
    eq(__LOC__, float_greaterequal(4.2, nan), false)
    eq(__LOC__, generic_greaterequal(4.2, nan), false)
    eq(__LOC__, float_greaterequal(nan, 4.2), false)
    eq(__LOC__, generic_greaterequal(nan, 4.2), false)
  })
})
