open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("max_int", () => eq(__LOC__, Int.Constants.maxValue, %raw("2147483647")))
  test("min_int", () => eq(__LOC__, Int.Constants.minValue, %raw("-2147483648")))
})
