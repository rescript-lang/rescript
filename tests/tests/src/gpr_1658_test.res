open Mocha
open Test_utils

describe(__LOC__, () => {
  test("JS Null operations", () => {
    eq(__LOC__, Js.Null.empty, Js.Null.empty)
    switch Js.Types.classify(Js.Null.empty) {
    | JSNull => eq(__LOC__, true, true)
    | _ => eq(__LOC__, true, false)
    }
    eq(__LOC__, true, Js.Types.test(Js.Null.empty, Null))
  })
})
