open Mocha
open Test_utils

let check_healty = check => !check["a"] && (!check["b"] && !check["c"])

let basic_not = x => !x

let f = check => check["x"] && check["y"]
/* [x && y] in OCaml can be translated to [x && y] in JS */

describe(__MODULE__, () => {
  test("f function", () => eq(__LOC__, false, f({"x": true, "y": false})))
  test("check_healty function", () =>
    eq(__LOC__, false, check_healty({"a": false, "b": false, "c": true}))
  )
})
