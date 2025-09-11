open Mocha
open Test_utils

let testIdentity = x =>
  switch x {
  | #1(x) => #1(x)
  | #2(x) => #2(x)
  }

describe(__MODULE__, () => {
  test("polymorphic variant identity test", () => {
    eq(__LOC__, testIdentity(#1(3)), #1(3))
    eq(__LOC__, testIdentity(#2(3)), #2(3))
  })
})
