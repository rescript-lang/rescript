open Mocha
open Test_utils

type t =
  | Required
  | Optional

let nextFor = (x: option<t>) =>
  switch x {
  | Some(Required) => Some(Optional)
  | Some(Optional) => None
  | None => Some(Required)
  }

describe(__MODULE__, () => {
  test("option type pattern matching", () => {
    eq(__LOC__, nextFor(Some(Required)), Some(Optional))
  })
})
