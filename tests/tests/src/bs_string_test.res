open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("string split and reduce", () =>
    eq(
      __LOC__,
      "ghso ghso g"->String.split(" ")->Array.reduce("", (x, y) => x ++ ("-" ++ y)),
      "-ghso-ghso-g",
    )
  )
})
