open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("keyword property plus with reduce", () => {
    eq(
      __LOC__,
      Js.Array2.reduce([1, 2, 3, 4], (x, y) => x + y, 0),
      {
        open Ident_mangles
        __dirname + __filename + exports + require
      },
    )
  })
})
