open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("string split and reduce", () =>
    eq(
      __LOC__,
      "ghso ghso g"->Js.String2.split(" ")->Js.Array2.reduce((x, y) => x ++ ("-" ++ y), ""),
      "-ghso-ghso-g",
    )
  )
})
