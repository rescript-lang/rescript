open Mocha
open Test_utils

module I = Belt.Int

describe(__MODULE__, () => {
  test("toFloat", () => {
    eq(__LOC__, I.toFloat(1), 1.0)
    eq(__LOC__, I.toFloat(-1), -1.0)
  })

  test("fromFloat", () => {
    eq(__LOC__, I.fromFloat(1.0), 1)
    eq(__LOC__, I.fromFloat(1.3), 1)
    eq(__LOC__, I.fromFloat(1.7), 1)
    eq(__LOC__, I.fromFloat(-1.0), -1)
    eq(__LOC__, I.fromFloat(-1.5), -1)
    eq(__LOC__, I.fromFloat(-1.7), -1)
  })

  test("fromString", () => {
    eq(__LOC__, I.fromString("1"), Some(1))
    eq(__LOC__, I.fromString("-1"), Some(-1))
    eq(__LOC__, I.fromString("1.7"), Some(1))
    eq(__LOC__, I.fromString("-1.0"), Some(-1))
    eq(__LOC__, I.fromString("-1.5"), Some(-1))
    eq(__LOC__, I.fromString("-1.7"), Some(-1))
    eq(__LOC__, I.fromString("not an int"), None)
  })

  test("toString", () => {
    eq(__LOC__, I.toString(1), "1")
    eq(__LOC__, I.toString(-1), "-1")
  })

  test("operators", () => {
    open! I

    eq(__LOC__, 2 + 3, 5)
    eq(__LOC__, 2 - 3, -1)
    eq(__LOC__, 2 * 3, 6)
    eq(__LOC__, 2 / 3, 0)
    eq(__LOC__, 2 % 2, 0)
    eq(__LOC__, (~-2), 1)
    eq(__LOC__, 2 ^ (~2), -1)
    eq(__LOC__, 2 ^ 3, 1)
    eq(__LOC__, 2 << 3, 16)
    eq(__LOC__, 16 >> 3, 2)
    eq(__LOC__, 16 >>> 3, 2)
  })
})
