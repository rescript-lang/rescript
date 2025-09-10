open Mocha
open Test_utils

@module("./joinClasses.mjs") @variadic external joinClasses: array<int> => int = "default"

let a = joinClasses([1, 2, 3])

let () = {
  let pair = (a, 6)
  Js.log(pair)
}

describe(__MODULE__, () => {
  test("joinClasses module splice", () => eq(__LOC__, a, 6))
})
