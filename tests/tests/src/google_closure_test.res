open Mocha
open Test_utils

describe("Closure", () => {
  test("partial", () =>
    eq(
      __LOC__,
      {
        open Test_google_closure
        (a, b, c)
      },
      ("3", 101, [1, 2]),
    )
  )
})
