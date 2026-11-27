open Mocha
open Test_utils

// A local module named JsError must not change what %todo expands to.
module JsError = {
  let throwWithMessage = (s: string) => s
}

let notImplemented = (_: int): int => %todo("later")

describe(__MODULE__, () => {
  test("%todo throws even with a local JsError module", () => {
    throws(__LOC__, () => ignore(notImplemented(1)))
  })
})
