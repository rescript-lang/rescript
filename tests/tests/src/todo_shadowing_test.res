// Every %todo reports warning 110, which this test exercises on purpose.
@@warning("-110")

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
