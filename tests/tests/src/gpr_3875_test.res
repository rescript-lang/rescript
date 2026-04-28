open Mocha
open Test_utils

let result = ref("")
/** TODO: 
  pattern match over (Some \"xx\") could be simplified
*/
module Xx = {
  let log = x => result := x
}

/** TODO: 
  pattern match over (Some \"xx\") could be simplified
*/
let compilerBug = (a, b, c, f) =>
  switch (a, b) {
  | (Some("x"), _)
  | (_, Some("x")) =>
    if f() {
      Xx.log("Some x, f returns true")
    } else {
      Xx.log("Some x, f returns false")
    }
  | _ =>
    if c {
      Xx.log("No x, c is true")
    } else {
      Xx.log("No x, c is false")
    }
  }

describe(__MODULE__, () => {
  test("compiler bug pattern matching", () => {
    compilerBug(Some("x"), None, true, () => true)
    eq(__LOC__, result.contents, "Some x, f returns true")
  })
})
