let v = %raw(`Number.EPSILON?Number.EPSILON:2.220446049250313e-16`)

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("epsilon", () => {
    eq(__LOC__, epsilon_float, v)
  })
  test("raw_epsilon", () => {
    eq(__LOC__, 2.220446049250313e-16, v)
  })
})
