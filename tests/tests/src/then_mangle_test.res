@@config(no_export)

open Mocha
open Test_utils

let then = (a, b) => {
  Js.log("no inline")
  a * a + b * b
}

describe(__MODULE__, () => {
  test("then function mangling", () => {
    eq(__LOC__, then(1, 2), 5)
  })
})
