open Mocha
open Test_utils

let u = () =>
  switch String.length("123") {
  | n => 3 / 0
  | exception _ => 42
  } /* TODO: could be optimized */

describe(__MODULE__, () => {
  test("jsoo_400 exception handling test", () => {
    throws(__LOC__, _ => ignore(u()))
  })
})
