open Mocha
open Test_utils

@unboxed
type color =
  | @as("primary") Primary
  | @as("secondary") Secondary
  | Color(string)

let colorName = value =>
  switch value {
  | Color(name) => name
  | _ => "not Color"
  }

let isPrimary = value =>
  switch value {
  | Primary => true
  | _ => false
  }

@unboxed
type number = | @as(1) One | Number(int)

let numberName = value =>
  switch value {
  | One => "one"
  | Number(_) => "number"
  }

let foldedPrimary = colorName(Color("primary"))
let foldedSecondary = colorName(Color("secondary"))
let foldedBlue = colorName(Color("blue"))
let foldedDefault = isPrimary(Color("primary"))
let foldedNumber = numberName(Number(1))
let foldedOtherNumber = numberName(Number(2))

// Keep runtime calls across an opaque boundary for comparison with inlining.
@inline(never)
let runtimeColorName = value => colorName(value)
@inline(never)
let runtimeNumberName = value => numberName(value)

// Values flowing through bindings must preserve the same behavior.
let primary = Color("primary")
let throughBinding = colorName(primary)

// Ordinary boxed variants must keep their distinct constructor identity.
type boxed = | @as("primary") BoxedPrimary | BoxedColor(string)
let boxedName = value =>
  switch value {
  | BoxedPrimary => "not Color"
  | BoxedColor(name) => name
  }
let foldedBoxed = boxedName(BoxedColor("primary"))

describe(__MODULE__, () => {
  test("unboxed variant folding agrees with runtime literal dispatch", () => {
    eq(__LOC__, foldedPrimary, "not Color")
    eq(__LOC__, foldedSecondary, "not Color")
    eq(__LOC__, foldedBlue, "blue")
    eq(__LOC__, foldedDefault, true)
    eq(__LOC__, foldedNumber, "one")
    eq(__LOC__, foldedOtherNumber, "number")
    eq(__LOC__, foldedPrimary, runtimeColorName(Color("primary")))
    eq(__LOC__, foldedNumber, runtimeNumberName(Number(1)))
    eq(__LOC__, throughBinding, "not Color")
    eq(__LOC__, foldedBoxed, "primary")
  })
})
