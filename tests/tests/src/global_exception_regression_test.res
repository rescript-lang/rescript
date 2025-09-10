open Mocha
open Test_utils

let v = Not_found

let u = Not_found
let s = End_of_file

describe(__MODULE__, () => {
  test("not_found_equal", () => {
    eq(__LOC__, u, v)
  })
  test("not_found_not_equal_end_of_file", () => {
    eq(__LOC__, u != s, true)
  })
})
