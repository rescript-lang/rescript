open Mocha
open Test_utils

// A constructor without @as is represented by its name, so `@as("A")` on a
// constructor named A states the same representation

type named = A | B
type declared = | @as("A") A | B | C

let declaredA = ((A: named) :> declared)

module Stated: {
  type t = A | B
  let a: t
} = {
  type t = | @as("A") A | B
  let a = A
}

describe(__MODULE__, () => {
  test("coercion", () => {
    eq(__LOC__, declaredA, A)
  })

  test("interface", () => {
    eq(__LOC__, Stated.a, Stated.A)
  })
})
