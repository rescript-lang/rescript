open Mocha
open Test_utils

module rec Int3: {
  let u: int => int
} = Int3

module Fact = {
  module type S = {
    let fact: int => int
  }
  module rec M: S = {
    let fact = n =>
      if n <= 1 {
        1
      } else {
        n * M.fact(n - 1)
      }
  }
  include M
}

describe(__MODULE__, () => {
  test("recursive module factorial", () => {
    eq(__LOC__, 120, Fact.fact(5))
  })

  test("recursive module exception", () => {
    throws(__LOC__, () => ignore(Int3.u(3)))
  })
})
