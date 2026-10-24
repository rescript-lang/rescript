open Mocha
open Test_utils

// In a variant with some @unboxed cases, the @as of an untagged case does not
// change its representation, so it does not clash with a tagged case's tag
// (#9020)
type t = | @unboxed @as("x") A(int) | @as("x") B({y: int}) | C

let kind = value =>
  switch value {
  | A(_) => "a"
  | B(_) => "b"
  | C => "c"
  }

@inline(never) let id = x => x

describe(__MODULE__, () => {
  test("an untagged case's @as is not a tag", () => {
    eq(__LOC__, kind(id(A(1))), "a")
    eq(__LOC__, kind(id(B({y: 1}))), "b")
    eq(__LOC__, kind(id(C)), "c")
  })
})
