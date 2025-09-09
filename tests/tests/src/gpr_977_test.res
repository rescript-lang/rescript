open Mocha
open Test_utils

let f = x => {
  for i in 0 to 100 {
    Js.log(".") /* prevent optimization */
  }
  -x
}

let min_32_int = -2147483648
let u = f(min_32_int)

describe(__MODULE__, () => {
  test("min 32 int function", () => eq(__LOC__, min_32_int, u))
})
