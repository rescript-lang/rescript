open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("weak array commented out tests", () => {
    // Tests are commented out, just verify compilation
    eq(__LOC__, 1, 1)
  })
})
