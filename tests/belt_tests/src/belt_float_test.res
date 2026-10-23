open Mocha
open Test_utils

module F = Belt.Float

describe(__MODULE__, () => {
  test("fromInt", () => {
    eq(__LOC__, F.fromInt(1), 1.0)
    eq(__LOC__, F.fromInt(-1), -1.0)
  })

  test("toInt", () => {
    eq(__LOC__, F.toInt(1.0), 1)
    eq(__LOC__, F.toInt(1.3), 1)
    eq(__LOC__, F.toInt(1.7), 1)
    eq(__LOC__, F.toInt(-1.0), -1)
    eq(__LOC__, F.toInt(-1.5), -1)
    eq(__LOC__, F.toInt(-1.7), -1)
  })

  test("fromString", () => {
    eq(__LOC__, F.fromString("1"), Some(1.0))
    eq(__LOC__, F.fromString("-1"), Some(-1.0))
    eq(__LOC__, F.fromString("1.7"), Some(1.7))
    eq(__LOC__, F.fromString("-1.0"), Some(-1.0))
    eq(__LOC__, F.fromString("-1.5"), Some(-1.5))
    eq(__LOC__, F.fromString("-1.7"), Some(-1.7))
    eq(__LOC__, F.fromString("not a float"), None)
  })

  test("toString", () => {
    eq(__LOC__, F.toString(1.0), "1")
    eq(__LOC__, F.toString(-1.0), "-1")
    eq(__LOC__, F.toString(-1.5), "-1.5")
  })

  test("operators", () => {
    open! F
    eq(__LOC__, 2.0 + 3.0, 5.0)
    eq(__LOC__, 2.0 - 3.0, -1.0)
    eq(__LOC__, 2.0 * 3.0, 6.0)
    eq(__LOC__, 3.0 / 2.0, 1.5)
  })
})
