open Mocha
open Test_utils

type t
@new external makeDate: unit => t = "Date"

let f = () => {
  let x = makeDate()
  let y = makeDate()
  (y > x, y < x, true)
}
describe(__MODULE__, () => {
  test("date comparison test", () => {
    let (a0, a1, a2) = f()
    Js.log2(a0, a1)
    eq(__LOC__, a2, true)
  })
})
