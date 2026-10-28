// `x === c1 && x === c2` and `x !== c1 && x === c2` are simplified by
// comparing c1 and c2 as JavaScript values: equal float literals are equal,
// and so are 1 and 1.0.

let float_eq_eq = (x: float) => x == 2.5 && x == 2.5
let float_neq_eq = (x: float) => x != 2.5 && x == 2.5
let float_eq_neq = (x: float) => x == 2.5 && x != 2.5
let float_eq_other = (x: float) => x == 2.5 && x == 3.5
let float_neq_other = (x: float) => x != 2.5 && x == 3.5
let float_or = (x: float) => x != 2.5 || x != 2.5

@unboxed type int_tag = | @as(1) L1 | N(float) | S(string)
// The payload case is unused (warning 11): 1 always matches as L1.
@warning("-11")
let int_tag_shadowed = (v: int_tag) =>
  switch v {
  | N(1.0) => 4
  | _ => 6
  }

@unboxed type float_tag = | @as(2.5) L | M(float) | P(string)
@warning("-11")
let float_tag_shadowed = (v: float_tag) =>
  switch v {
  | M(2.5) => 4
  | _ => 6
  }

// Keeps the arguments unknown, so the calls are not folded to constants.
let opaque: 'a => 'a = %raw(`x => x`)

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("equal float literals", () => {
    eq(__LOC__, true, float_eq_eq(opaque(2.5)))
    eq(__LOC__, false, float_eq_eq(opaque(3.5)))
    eq(__LOC__, false, float_neq_eq(opaque(2.5)))
    eq(__LOC__, false, float_eq_neq(opaque(2.5)))
    eq(__LOC__, true, float_or(opaque(3.5)))
    eq(__LOC__, false, float_or(opaque(2.5)))
  })
  test("different float literals", () => {
    eq(__LOC__, false, float_eq_other(opaque(2.5)))
    eq(__LOC__, true, float_neq_other(opaque(3.5)))
    eq(__LOC__, false, float_neq_other(opaque(2.5)))
  })
  test("literal tag equal to a payload literal", () => {
    eq(__LOC__, 6, int_tag_shadowed(opaque(L1)))
    eq(__LOC__, 6, int_tag_shadowed(opaque(N(2.0))))
    eq(__LOC__, 6, float_tag_shadowed(opaque(L)))
    eq(__LOC__, 6, float_tag_shadowed(opaque(M(1.0))))
  })
})
