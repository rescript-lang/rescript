open Mocha
open Test_utils

module H = Gpr_3566_test.Test()

module Caml_option = {}
let f = x => Some(x)

describe(__MODULE__, () => {
  test("gpr_3566_drive_test", () => eq(__LOC__, H.b, true))
})
