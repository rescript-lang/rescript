open Mocha
open Test_utils

module type X = module type of String

let u = (v: module(X)) => v

module N = {
  let s = u(module(String))
}

let v0 = {
  module V = unpack(N.s: X)
  V.length("x")
}

let v = x => {
  module V = unpack(N.s: X)
  V.length(x)
}

describe(__MODULE__, () => {
  test("const", () => {
    eq(__LOC__, 1, v0)
  })

  test("other", () => {
    eq(__LOC__, 3, v("abc"))
  })
})
