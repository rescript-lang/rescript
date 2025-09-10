open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("max_int", () => eq(__LOC__, max_int, %raw("2147483647")))
  test("min_int", () => eq(__LOC__, min_int, %raw("-2147483648")))
})
