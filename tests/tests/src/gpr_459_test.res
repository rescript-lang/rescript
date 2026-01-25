@@config({no_export: no_export})

open Mocha
open Test_utils

let uu = {
  "_'x": 3,
}

let uu2 = {
  "_then": 1,
  "catch": 2,
  "_'x": 3,
}

let hh = uu["_'x"]

describe(__MODULE__, () => {
  test("access object property with underscore", () => eq(__LOC__, hh, 3))
  test("access multiple object properties", () =>
    eq(__LOC__, (1, 2, 3), (uu2["_then"], uu2["catch"], uu2["_'x"]))
  )
})
