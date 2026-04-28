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
    eq(__LOC__, Js.Float.fromString("3.0"), 3.0)
    eq(__LOC__, float_compare(Js.Float._NaN, Js.Float._NaN), 0)
    eq(__LOC__, generic_compare(Js.Float._NaN, Js.Float._NaN), 0)
    eq(__LOC__, float_compare(Js.Float._NaN, neg_infinity), -1)
    eq(__LOC__, generic_compare(Js.Float._NaN, neg_infinity), -1)
    eq(__LOC__, float_compare(neg_infinity, Js.Float._NaN), 1)
    eq(__LOC__, generic_compare(neg_infinity, Js.Float._NaN), 1)
    eq(__LOC__, float_equal(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, generic_equal(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, float_equal(4.2, Js.Float._NaN), false)
    eq(__LOC__, generic_equal(4.2, Js.Float._NaN), false)
    eq(__LOC__, float_equal(Js.Float._NaN, 4.2), false)
    eq(__LOC__, generic_equal(Js.Float._NaN, 4.2), false)
    eq(__LOC__, float_notequal(Js.Float._NaN, Js.Float._NaN), true)
    eq(__LOC__, generic_notequal(Js.Float._NaN, Js.Float._NaN), true)
    eq(__LOC__, float_notequal(4.2, Js.Float._NaN), true)
    eq(__LOC__, generic_notequal(4.2, Js.Float._NaN), true)
    eq(__LOC__, float_notequal(Js.Float._NaN, 4.2), true)
    eq(__LOC__, generic_notequal(Js.Float._NaN, 4.2), true)
    eq(__LOC__, float_lessthan(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, generic_lessthan(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, float_lessthan(4.2, Js.Float._NaN), false)
    eq(__LOC__, generic_lessthan(4.2, Js.Float._NaN), false)
    eq(__LOC__, float_lessthan(Js.Float._NaN, 4.2), false)
    eq(__LOC__, generic_lessthan(Js.Float._NaN, 4.2), false)
    eq(__LOC__, float_greaterthan(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, generic_greaterthan(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, float_greaterthan(4.2, Js.Float._NaN), false)
    eq(__LOC__, generic_greaterthan(4.2, Js.Float._NaN), false)
    eq(__LOC__, float_greaterthan(Js.Float._NaN, 4.2), false)
    eq(__LOC__, generic_greaterthan(Js.Float._NaN, 4.2), false)
    eq(__LOC__, float_lessequal(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, generic_lessequal(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, generic_lessequal(4.2, Js.Float._NaN), false)
    eq(__LOC__, generic_lessequal(4.2, Js.Float._NaN), false)
    eq(__LOC__, generic_lessequal(Js.Float._NaN, 4.2), false)
    eq(__LOC__, generic_lessequal(Js.Float._NaN, 4.2), false)
    eq(__LOC__, float_greaterequal(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, generic_greaterequal(Js.Float._NaN, Js.Float._NaN), false)
    eq(__LOC__, float_greaterequal(4.2, Js.Float._NaN), false)
    eq(__LOC__, generic_greaterequal(4.2, Js.Float._NaN), false)
    eq(__LOC__, float_greaterequal(Js.Float._NaN, 4.2), false)
    eq(__LOC__, generic_greaterequal(Js.Float._NaN, 4.2), false)
  })
})
