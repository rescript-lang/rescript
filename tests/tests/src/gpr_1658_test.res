open Mocha
open Test_utils

describe(__LOC__, () => {
  test("JS Null operations", () => {
    eq(__LOC__, Null.null, Null.null)
    switch Null.null->Type.Classify.classify {
    | Null => eq(__LOC__, true, true)
    | _ => eq(__LOC__, true, false)
    }
    eq(
      __LOC__,
      true,
      switch Null.null->Type.Classify.classify {
      | Null => true
      | _ => false
      },
    )
  })
})
