open Mocha
open Test_utils
open Js

exception A(int)
exception B
exception C(int, int)

let test_js_error4 = () =>
  try {
    ignore(Js.Json.parseExn(` {"x"}`))
    1
  } catch {
  | Not_found => 2
  | Invalid_argument("x") => 3
  | A(2) => 4
  | B => 5
  | C(1, 2) => 6
  | e => 7
  }

let f = g =>
  try g() catch {
  | Not_found => 1
  }

describe(__MODULE__, () => {
  test("exception rebound error test", () => {
    eq(__LOC__, test_js_error4(), 7)
  })
})
