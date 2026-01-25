open Mocha
open Test_utils

let ff = v => Js.Float.toString(v)
let f = v => Js.Int.toString(v)

describe(__MODULE__, () => {
  test("infinity to string", () => eq(__LOC__, ff(infinity), "Infinity"))
  test("neg_infinity to string", () => eq(__LOC__, ff(neg_infinity), "-Infinity"))
})
