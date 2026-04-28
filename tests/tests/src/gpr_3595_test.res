open Mocha
open Test_utils

@@warning("-a")

/* let [|a|] = [|1|] */

let x = 1

describe(__MODULE__, () => {
  test("commented out pattern test", () => {
    // This test is mostly commented out, just checking compilation
    eq(__LOC__, x, 1)
  })
})
