open Mocha
open Test_utils

let y = switch failwith("boo") {
| exception Failure(msg) => Some(msg)
| e => None
}

let x = switch failwith("boo") {
| exception Failure(msg) => Some(msg)
| e =>
  Console.log("ok")
  None
}

describe(__MODULE__, () => {
  test("gpr_2316_test", () => {
    eq(__LOC__, y, Some("boo"))
    eq(__LOC__, x, Some("boo"))
  })
})
