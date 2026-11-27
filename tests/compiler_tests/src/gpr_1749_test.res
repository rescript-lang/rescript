open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("gpr_1749_test", () => {
    let a = if 1. < (1. < 1. ? 1. : 10.) {
      0
    } else {
      1
    }

    eq(__LOC__, 0, a)
  })
})
