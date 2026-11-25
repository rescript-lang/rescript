open Mocha
open Test_utils

/***
A block is compiled from its tag, its constructor name or record field names,
and its fields. The tag is the constructor's position in its type, so after a
coercion two blocks with the same tag and fields can still compile to different
values. Merging switch arms or `if` branches that return blocks must compare
their names too.

Each function is called with a constant, which the optimizer may fold, and
through [id], whose [@inline(never)] leaves the call to the runtime.
*/

@inline(never) let id = x => x

type source = X | Y | Z

// `Second` is at position 0 in `smallBlock`, as `First` is in `bigBlock`.
type smallBlock = Second(int)
type bigBlock = First(int) | Second(int)

// The arms for `X` and `Z` may be merged, but not with the arm for `Y`.
let toBigBlock = (x: source): bigBlock =>
  switch x {
  | X => First(1)
  | Y => (Second(1): smallBlock :> bigBlock)
  | Z => First(1)
  }

// The same values, in fields of other names.
type xy = {x: int, y: int}
type yx = {y: int, x: int}

// The arms for `X` and `Z` may be merged, but not with the arm for `Y`.
let toYx = (x: source): yx =>
  switch x {
  | X => {y: 1, x: 2}
  | Y => ({x: 1, y: 2}: xy :> yx)
  | Z => {y: 1, x: 2}
  }

// The same, through an `if` rather than a `switch`.
let toBigBlockIf = (b: bool): bigBlock => b ? First(1) : (Second(1): smallBlock :> bigBlock)

// The same, with a payload that isn't a constant.
let toBigBlockIfPayload = (b: bool, n: int): bigBlock =>
  b ? First(n) : (Second(n): smallBlock :> bigBlock)

describe(__MODULE__, () => {
  test("switch arms returning constructors of coerced types", () => {
    eq(__LOC__, toBigBlock(X), First(1))
    eq(__LOC__, toBigBlock(Y), Second(1))
    eq(__LOC__, toBigBlock(id(X)), First(1))
    eq(__LOC__, toBigBlock(id(Y)), Second(1))
    eq(__LOC__, toBigBlock(id(Z)), First(1))
  })

  test("switch arms returning records of coerced types", () => {
    eq(__LOC__, toYx(Y), {y: 2, x: 1})
    eq(__LOC__, toYx(id(X)), {y: 1, x: 2})
    eq(__LOC__, toYx(id(Y)), {y: 2, x: 1})
    eq(__LOC__, toYx(id(Z)), {y: 1, x: 2})
  })

  test("if branches returning constructors of coerced types", () => {
    eq(__LOC__, toBigBlockIf(true), First(1))
    eq(__LOC__, toBigBlockIf(false), Second(1))
    eq(__LOC__, toBigBlockIf(id(true)), First(1))
    eq(__LOC__, toBigBlockIf(id(false)), Second(1))
  })

  test("if branches returning constructors with a payload that isn't a constant", () => {
    eq(__LOC__, toBigBlockIfPayload(true, 1), First(1))
    eq(__LOC__, toBigBlockIfPayload(false, 1), Second(1))
    eq(__LOC__, toBigBlockIfPayload(id(true), id(1)), First(1))
    eq(__LOC__, toBigBlockIfPayload(id(false), id(1)), Second(1))
  })
})
