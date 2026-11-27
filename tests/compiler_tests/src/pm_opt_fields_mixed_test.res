// Patterns of an optional field that test its value directly (`{x: 1}`) and
// patterns that test whether it is present (`{x: ?None}`, `{x: ?Some(v)}`) in
// one match.

type r = {x?: int, y?: int}

let valueThenNone = (r: r) =>
  switch r {
  | {x: 1} => "x is 1"
  | {y: 2} => "y is 2"
  | {x: ?None} => "no x"
  | _ => "other"
  }

let noneThenValue = (r: r) =>
  switch r {
  | {x: ?None} => "no x"
  | {x: 1} => "x is 1"
  | {x: ?Some(_)} => "some x"
  }

let someThenValue = (r: r) =>
  switch r {
  | {x: ?Some(v), y: 1} => v
  | {x: 5} => 100
  | _ => 0
  }

let dictValueThenNone = (d: dict<int>) =>
  switch d {
  | dict{"x": 1} => "x is 1"
  | dict{"y": 2} => "y is 2"
  | dict{"x": ?None} => "no x"
  | _ => "other"
  }

let dictSomeThenValue = (d: dict<int>) =>
  switch d {
  | dict{"x": ?Some(v), "y": 1} => v
  | dict{"x": 5} => 100
  | _ => 0
  }

// Untagged variant cases that name every payload case still leave the field
// absent.
@unboxed type payload = String(string) | Number(float)
type withPayload = {p?: payload}

let allPayloadCases = (r: withPayload) =>
  switch r {
  | {p: String(_)} => "string"
  | {p: Number(_)} => "number"
  | _ => "absent"
  }

@unboxed type withConstant = A | String(string) | Number(float)
type withConstantField = {c?: withConstant}

let allPayloadCasesWithConstant = (r: withConstantField) =>
  switch r {
  | {c: String(_)} => "string"
  | {c: Number(_)} => "number"
  | _ => "other"
  }

// Two variants with a case of the same name, on fields of the same name
@unboxed type oneCase = Foo(string)
@unboxed type twoCases = Foo(string) | Bar(float)
type withOneCase = {x?: oneCase}
type withTwoCases = {x?: twoCases}

let sharedCaseName = (p: (withOneCase, withTwoCases)) =>
  switch p {
  | ({x: Foo(_)}, _) => "first"
  | (_, {x: Foo(_)}) => "second"
  | _ => "other"
  }

// A field whose type has an undefined case: `Some(Undefined)` is represented
// by an object, which only some payload patterns can tell apart
@unboxed
type withUndefined =
  | @as(undefined) Undefined
  | @as(null) Null
  | String(string)
  | Object(dict<string>)

type withUndefinedField = {u?: withUndefined}

// Through a ref, so that `Some(Undefined)` is built at runtime
let undefinedCase = ref(Undefined)
let withU = (v: withUndefined): withUndefinedField => {u: v}

let stringOrOther = (r: withUndefinedField) =>
  switch r {
  | {u: String(s)} => s
  | {u: Null} => "null"
  | _ => "other"
  }

let objectOrOther = (r: withUndefinedField) =>
  switch r {
  | {u: Object(_)} => "object"
  | {u: Undefined} => "undefined"
  | _ => "other"
  }

let objectOnly = (r: withUndefinedField) =>
  switch r {
  | {u: Object(_)} => "object"
  | _ => "other"
  }

let boundPayload = (r: withUndefinedField) =>
  switch r {
  | {u: ?Some(v)} => v == Undefined ? "undefined" : "other value"
  | _ => "absent"
  }

// Options matched against constants, without the presence test
let optionInt = (o: option<int>) =>
  switch o {
  | Some(1) => "one"
  | _ => "other"
  }

let optionString = (o: option<string>) =>
  switch o {
  | Some("x") => 1
  | Some("y") => 2
  | _ => 3
  }

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("value pattern, then ?None on the same field", () => {
    eq(__LOC__, valueThenNone({x: 1}), "x is 1")
    eq(__LOC__, valueThenNone({y: 2}), "y is 2")
    eq(__LOC__, valueThenNone({}), "no x")
    eq(__LOC__, valueThenNone({x: 3}), "other")
  })
  test("?None, then a value pattern on the same field", () => {
    eq(__LOC__, noneThenValue({}), "no x")
    eq(__LOC__, noneThenValue({x: 1}), "x is 1")
    eq(__LOC__, noneThenValue({x: 2}), "some x")
  })
  test("?Some(v), then a value pattern on the same field", () => {
    eq(__LOC__, someThenValue({x: 7, y: 1}), 7)
    eq(__LOC__, someThenValue({x: 5}), 100)
    eq(__LOC__, someThenValue({x: 5, y: 2}), 100)
    eq(__LOC__, someThenValue({}), 0)
  })
  test("dict patterns", () => {
    eq(__LOC__, dictValueThenNone(dict{"x": 1}), "x is 1")
    eq(__LOC__, dictValueThenNone(dict{"y": 2}), "y is 2")
    eq(__LOC__, dictValueThenNone(dict{}), "no x")
    eq(__LOC__, dictValueThenNone(dict{"x": 3}), "other")
    eq(__LOC__, dictSomeThenValue(dict{"x": 7, "y": 1}), 7)
    eq(__LOC__, dictSomeThenValue(dict{"x": 5}), 100)
    eq(__LOC__, dictSomeThenValue(dict{}), 0)
  })
  test("untagged variant cases", () => {
    eq(__LOC__, allPayloadCases({p: String("a")}), "string")
    eq(__LOC__, allPayloadCases({p: Number(1.)}), "number")
    eq(__LOC__, allPayloadCases({}), "absent")
    eq(__LOC__, allPayloadCasesWithConstant({c: String("a")}), "string")
    eq(__LOC__, allPayloadCasesWithConstant({c: Number(1.)}), "number")
    eq(__LOC__, allPayloadCasesWithConstant({c: A}), "other")
    eq(__LOC__, allPayloadCasesWithConstant({}), "other")
    eq(__LOC__, sharedCaseName(({x: Foo("a")}, {})), "first")
    eq(__LOC__, sharedCaseName(({}, {x: Foo("a")})), "second")
    eq(__LOC__, sharedCaseName(({}, {x: Bar(1.)})), "other")
    eq(__LOC__, sharedCaseName(({}, {})), "other")
  })
  test("a field whose type has an undefined case", () => {
    eq(__LOC__, stringOrOther({u: String("a")}), "a")
    eq(__LOC__, stringOrOther({u: Null}), "null")
    eq(__LOC__, stringOrOther(withU(undefinedCase.contents)), "other")
    eq(__LOC__, stringOrOther({}), "other")
    eq(__LOC__, objectOrOther({u: Object(dict{})}), "object")
    eq(__LOC__, objectOrOther(withU(undefinedCase.contents)), "undefined")
    eq(__LOC__, objectOrOther({u: String("a")}), "other")
    eq(__LOC__, objectOrOther({}), "other")
    eq(__LOC__, objectOnly(withU(Object(dict{}))), "object")
    eq(__LOC__, objectOnly(withU(undefinedCase.contents)), "other")
    eq(__LOC__, objectOnly({}), "other")
    eq(__LOC__, boundPayload(withU(undefinedCase.contents)), "undefined")
    eq(__LOC__, boundPayload({u: String("a")}), "other value")
    eq(__LOC__, boundPayload({}), "absent")
  })
  test("options matched against constants", () => {
    eq(__LOC__, optionInt(Some(1)), "one")
    eq(__LOC__, optionInt(Some(2)), "other")
    eq(__LOC__, optionInt(None), "other")
    eq(__LOC__, optionString(Some("x")), 1)
    eq(__LOC__, optionString(Some("y")), 2)
    eq(__LOC__, optionString(Some("z")), 3)
    eq(__LOC__, optionString(None), 3)
  })
})
