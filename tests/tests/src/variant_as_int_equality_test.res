open Mocha
open Test_utils

/***
A constant constructor is compiled to its `@as` value, or to its name when it
has none. Folding an equality between constructors, or merging switch arms
that return them, must compare those values, not the constructor's position in
its type, which an `@as(int)` value can collide with.

Each comparison is computed with constants, which the optimizer folds, and
through [id], whose [@inline(never)] leaves the comparison to the runtime. The
compiled output next to this file shows which comparisons fold.
*/

@inline(never) let id = x => x

// `A` is the number 1 and `B` is the string "B" at position 1.
type mixed = | @as(1) A | B | C

// `U` is the number 1 and `V` is the string "x" at position 1.
type literals = | @as(1) U | @as("x") V

type numbers = | @as(1) One | @as(2) Two

type source = X | Y | Z

// `Escaped` is written with an escape, `Letter` without, but both are "A".
// `EscapedB` is "B".
type escaped = | @as("\u0041") Escaped | @as("\x42") EscapedB
@unboxed type letter = | @as("A") Letter | Text(string)

// The arms for `X` and `Z` may be merged, but not with the arm for `Y`.
let toMixed = (x: source): mixed =>
  switch x {
  | X => B
  | Y => A
  | Z => B
  }

describe(__MODULE__, () => {
  test("an @as(int) constructor and the constructor at its position", () => {
    eq(__LOC__, A == B, false)
    eq(__LOC__, A != B, true)
    eq(__LOC__, U == V, false)
    eq(__LOC__, id(A) == id(B), false)
    eq(__LOC__, id(U) == id(V), false)
  })

  test("constructors equal to themselves", () => {
    eq(__LOC__, A == A, true)
    eq(__LOC__, B == B, true)
    eq(__LOC__, B == C, false)
    eq(__LOC__, B != C, true)
    eq(__LOC__, id(B) == id(C), false)
  })

  test("a coerced @as(int) constructor and an int", () => {
    eq(__LOC__, (One :> int) == 1, true)
    eq(__LOC__, (Two :> int) == 1, false)
    eq(__LOC__, (id(One) :> int) == 1, true)
  })

  test("@as strings spelled differently with the same value", () => {
    eq(__LOC__, ((Escaped :> string) :> letter) == Letter, true)
    eq(__LOC__, ((Escaped :> string) :> letter) != Letter, false)
    eq(__LOC__, ((id(Escaped) :> string) :> letter) == Letter, true)
    eq(__LOC__, Escaped == EscapedB, false)
    eq(__LOC__, id(Escaped) == id(EscapedB), false)
  })

  test("switch arms returning different constructors", () => {
    eq(__LOC__, toMixed(X), B)
    eq(__LOC__, toMixed(Y), A)
    eq(__LOC__, toMixed(Z), B)
    eq(__LOC__, toMixed(id(X)), B)
    eq(__LOC__, toMixed(id(Y)), A)
    eq(__LOC__, toMixed(id(Z)), B)
  })
})
