open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("keyword property plus with reduce", () => {
    eq(
      __LOC__,
      Array.reduce([1, 2, 3, 4], 0, (x, y) => x + y),
      {
        open Ident_mangles
        __dirname + __filename + exports + require
      },
    )
  })
})
