open Mocha
open Test_utils

let f = x => ignore(x)

let ff = x => ignore(Js.log(x))

describe(__MODULE__, () => {
  test("ignore function", () => {
    eq(__LOC__, f(3), ())
  })
})
