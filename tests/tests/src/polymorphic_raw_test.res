@@config({
  flags: [],
})

open Mocha
open Test_utils

let f: _ => string = %raw(` (a) => typeof a  `)

let a = f(3)
let b = f("3")

describe(__MODULE__, () => {
  test("polymorphic raw test", () => {
    eq(__LOC__, a, "number")
    eq(__LOC__, b, "string")
  })
})
