open Mocha
open Test_utils

let length = _ => 3

/* Test name collision */
describe(__MODULE__, () => {
  test("list_length", () => eq(__LOC__, 2, Belt.List.length(list{1, 2})))
  test("length", () => eq(__LOC__, 3, length(list{1, 2})))
})
