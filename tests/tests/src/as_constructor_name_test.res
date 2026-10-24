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

// Literals are compared by runtime value, also when a match is folded
type sourceNumber = | @as(1.) One
type targetNumber = | @as(1.0) One | Two | Three | Four

let isOne = (x: targetNumber) =>
  switch x {
  | One => true
  | _ => false
  }

type sourceBlock = | @as(1.) OneOf(int)
type targetBlock = | @as(1.0) OneOf(int) | TwoOf(int)

let isOneOf = (x: targetBlock) =>
  switch x {
  | OneOf(_) => true
  | _ => false
  }

describe(__MODULE__, () => {
  test("coercion", () => {
    eq(__LOC__, declaredA, A)
  })

  test("folded matches of coerced constants", () => {
    eq(__LOC__, isOne((One: sourceNumber) :> targetNumber), true)
    eq(__LOC__, isOneOf((OneOf(1): sourceBlock) :> targetBlock), true)
  })

  test("interface", () => {
    eq(__LOC__, Stated.a, Stated.A)
  })
})
