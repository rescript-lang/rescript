open Js_obj
open Mocha
open Test_utils

type x = {"say": int => int}

describe(__MODULE__, () => {
  test("empty", () => {
    eq(__LOC__, 0, Belt.Array.length(keys(empty())))
  })

  test("assign", () => {
    eq(__LOC__, {"a": 1}, assign(empty(), {"a": 1}))
  })
})
