open Mocha
open Test_utils

// A bool payload receives the booleans that are not literal cases, also in a
// match that does not name every case (#9035)

@unboxed type withTrue = | @as(true) LitTrue | TrueBool(bool) | TrueInt(int)
@unboxed type withFalse = | @as(false) LitFalse | FalseBool(bool) | FalseInt(int)

@inline(never) let id = x => x

let trueKind = (v: withTrue) =>
  switch v {
  | TrueBool(_) => "bool"
  | _ => "other"
  }

let falseKind = (v: withFalse) =>
  switch v {
  | FalseBool(_) => "bool"
  | _ => "other"
  }

describe(__MODULE__, () => {
  test("beside @as(true)", () => {
    eq(__LOC__, trueKind(id(TrueBool(false))), "bool")
    eq(__LOC__, trueKind(id(LitTrue)), "other")
    eq(__LOC__, trueKind(id(TrueInt(1))), "other")
  })

  test("beside @as(false)", () => {
    eq(__LOC__, falseKind(id(FalseBool(true))), "bool")
    eq(__LOC__, falseKind(id(LitFalse)), "other")
    eq(__LOC__, falseKind(id(FalseInt(1))), "other")
  })
})
