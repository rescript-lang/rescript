open Mocha
open Test_utils

// Checks against both boolean literals must not assume that the value is a
// boolean: in an untagged variant it can be any of the payloads (#9098)

@unboxed type withBoth = D(Date.t) | @as(true) Lt | @as(false) Lf | R(RegExp.t)
@unboxed type withTrue = Bo(bool) | S(string) | @as(true) LitTrue

@inline(never) let id = x => x

let bothKind = (v: withBoth) =>
  switch v {
  | D(_) => "date"
  | _ => "other"
  }

let literalKind = (v: withBoth) =>
  switch v {
  | Lt | Lf => "literal"
  | _ => "other"
  }

let falseKind = (v: withTrue) =>
  switch v {
  | Bo(false) => "false"
  | _ => "other"
  }

describe(__MODULE__, () => {
  test("no case names a boolean literal", () => {
    eq(__LOC__, bothKind(id(D(Date.make()))), "date")
    eq(__LOC__, bothKind(id(R(/a/))), "other")
    eq(__LOC__, bothKind(id(Lt)), "other")
    eq(__LOC__, bothKind(id(Lf)), "other")
  })

  test("both boolean literals in one case", () => {
    eq(__LOC__, literalKind(id(Lt)), "literal")
    eq(__LOC__, literalKind(id(Lf)), "literal")
    eq(__LOC__, literalKind(id(D(Date.make()))), "other")
    eq(__LOC__, literalKind(id(R(/a/))), "other")
  })

  test("bool payload beside @as(true)", () => {
    eq(__LOC__, falseKind(id(Bo(false))), "false")
    eq(__LOC__, falseKind(id(S("hello"))), "other")
    eq(__LOC__, falseKind(id(LitTrue)), "other")
  })
})
