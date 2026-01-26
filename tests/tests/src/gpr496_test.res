open Mocha
open Test_utils

let expected = (
  true == false,
  false == true,
  false == false,
  true == true,
  Pervasives.compare(false, true),
  Pervasives.compare(true, false),
  Pervasives.compare(false, false),
  Pervasives.compare(true, true),
)

let expected2 = (false, false, true, true, -1, 1, 0, 0)
let u = (
  true == false,
  false == true,
  false == false,
  true == true,
  Pervasives.compare(false, true),
  Pervasives.compare(true, false),
  Pervasives.compare(false, false),
  Pervasives.compare(true, true),
)

let ff = (x: bool, y) => min(x, y())

describe(__MODULE__, () => {
  test("expected equals u", () => eq(__LOC__, expected, u))
  test("expected equals expected2", () => eq(__LOC__, expected, expected2))
  test("min function", () => eq(__LOC__, false, min(true, false)))
})
