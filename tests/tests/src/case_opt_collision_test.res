open Mocha
open Test_utils

let f = (~x=3, y: int) => {
  let xOpt = x + 2
  Console.log(xOpt)
  xOpt + y
}

describe(__MODULE__, () => {
  test("optional parameter collision", () => {
    Console.log(f(2))
    eq(__LOC__, f(2), 7)
    eq(__LOC__, f(~x=4, 2), 8)
  })
})
