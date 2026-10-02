open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("getUnsafe", () => {
    eq(__LOC__, String.codePointAt("ghsogh", 3), Some(111))
    eq(__LOC__, String.codePointAt("ghsogh", -3), None)
  })
})
