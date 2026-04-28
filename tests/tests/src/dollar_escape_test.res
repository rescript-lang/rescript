open Mocha
open Test_utils

let \"$$" = (x, y) => x + y

let v = \"$$"(1, 2)

let \"$$+" = (x, y) => x * y

let u = \"$$+"(1, 3)

describe(__MODULE__, () => {
  test("dollar escape operators", () => {
    eq(__LOC__, v, 3)
    eq(__LOC__, u, 3)
  })
})
