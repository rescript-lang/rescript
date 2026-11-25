open Mocha
open Test_utils

/***
`Float.fromInt` of an int literal stays an int literal. Adding or
subtracting such literals as floats is exact: it is only folded when the
result still fits in an int, and never wraps like int arithmetic does.
A chain `x + k + j` is not reassociated, since float addition is inexact.
*/

@inline(never) let id = x => x

@unboxed type floatOrString = Float(float) | String(string)

describe(__MODULE__, () => {
  test("two literals", () => {
    eq(__LOC__, Float.fromInt(2147483647) +. Float.fromInt(1), 2147483648.0)
    eq(__LOC__, Float.fromInt(-2147483648) -. Float.fromInt(1), -2147483649.0)
    eq(__LOC__, Float.fromInt(2147483646) +. Float.fromInt(1), 2147483647.0)
  })

  test("a chain", () => {
    let x = id(10.0)
    eq(__LOC__, x +. Float.fromInt(2147483647) +. Float.fromInt(1), 2147483658.0)
    eq(__LOC__, x +. Float.fromInt(2147483647) +. Float.fromInt(2147483647), 4294967304.0)
    eq(__LOC__, x +. Float.fromInt(1) +. Float.fromInt(2), 13.0)
  })

  test("the smallest int", () => {
    eq(__LOC__, id(10.0) +. Float.fromInt(-2147483648), -2147483638.0)
    eq(__LOC__, id(10.0) +. Float.fromInt(-5), 5.0)
  })

  test("chains are not reassociated", () => {
    let x = id(0.1)
    eq(
      __LOC__,
      x +. Float.fromInt(-2147483648) +. Float.fromInt(2147483647),
      0.1 -. 2147483648.0 +. 2147483647.0,
    )
  })

  test("negating a negative operand", () => {
    eq(__LOC__, Float.fromInt(0) -. Float.fromInt(-2147483648), 2147483648.0)
    eq(__LOC__, Float.fromInt(0) -. -5.0, 5.0)
    let a = id(3.0)
    eq(__LOC__, Float.fromInt(0) -. (Float.fromInt(0) -. a) +. id(1.0), 4.0)
    eq(__LOC__, a, 3.0)
    eq(__LOC__, Float.fromInt(0) -. -0x1.8p3, 12.0)
    eq(__LOC__, Float.fromInt(0) -. (Obj.magic(-5n): float), (Obj.magic(5n): float))
  })

  test("negating a negative operand printed without its wrapper", () => {
    let a = id(3.0)
    eq(__LOC__, Float.fromInt(0) -. Option.getUnsafe(Some(Float.fromInt(0) -. a)), 3.0)
    eq(__LOC__, a, 3.0)
    eq(__LOC__, 0 - Option.getUnsafe(Some(-5)), 5)
    eq(__LOC__, Float.fromInt(0) -. Option.getUnsafe(Some(%raw("-5"))), 5.0)
    eq(__LOC__, Float.fromInt(0) -. (Obj.magic(Float(-5.0)): float), 5.0)
  })

  test("negating a raw negative literal", () => {
    eq(__LOC__, Float.fromInt(0) -. %raw("-5"), 5.0)
    eq(__LOC__, -(%raw("-5"): int), 5)
    eq(__LOC__, Float.fromInt(0) -. %raw("5"), -5.0)
  })

  test("int arithmetic wraps", () => {
    eq(__LOC__, Float.toInt(Float.fromInt(2147483647)) + 1, -2147483648)
    eq(__LOC__, Float.toInt(Float.fromInt(-2147483648)) - 1, 2147483647)
    eq(__LOC__, id(5) + -2147483648, -2147483643)
    eq(__LOC__, id(-5) + -2147483648, 2147483643)
    eq(__LOC__, id(-5) - -2147483648, 2147483643)
  })
})
