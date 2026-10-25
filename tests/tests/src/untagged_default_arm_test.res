open Mocha
open Test_utils

%%raw(`
globalThis.UntaggedAnyValue = class AnyValue {
  static [Symbol.hasInstance]() { return true }
}
`)

@inline(never) let id = x => x

// A switch with a default arm keeps the cases it does not name out of the
// arms it names (#8995)
@unboxed
type instances = D(Date.t) | R(RegExp.t) | O({x: int}) | S(string) | I(int) | F(int => int)

let objectOrString = value =>
  switch value {
  | O(_) => "O"
  | S(_) => "S"
  | _ => "other"
  }

@unboxed
type literals =
  | @as("lit") Lit | Text(string) | Count(int) | Flag(bool) | Rec({x: int}) | Fn(int => int)

let textOrRecord = value =>
  switch value {
  | Text(_) => "text"
  | Rec(_) => "record"
  | _ => "other"
  }

// A class whose Symbol.hasInstance accepts every value does not take
// primitives in the two-arm form or in the literal test
@instanceOf("UntaggedAnyValue") type anyValue

@unboxed
type twoArms = Any(anyValue) | Name(string)

let twoArmsKind = value =>
  switch value {
  | Any(_) => "any"
  | Name(_) => "name"
  }

@unboxed
type withLiterals =
  | @as(1) One
  | @as(2) Two
  | @as(3) Three
  | @as(4) Four
  | @as(5) Five
  | AnyLit(anyValue)
  | NameLit(string)

let withLiteralsKind = value =>
  switch value {
  | One => "one"
  | Two => "two"
  | Three => "three"
  | Four => "four"
  | Five => "five"
  | AnyLit(_) => "any"
  | NameLit(_) => "name"
  }

// The match on [b] is not complete: a value no arm takes falls through to the
// later arms
let pair = (a: JSON.t, b: JSON.t) =>
  switch (a, b) {
  | (String(_), Object(_)) => "string, object"
  | (_, Object(_)) => "any, object"
  | (Number(_), _) => "number, any"
  | _ => "other"
  }

// An arm that does nothing is still an arm: the string check stays
@unboxed type stringOrRecord = LogStr(string) | LogRec({x: int})

let logged = []
let logString = value => {
  switch value {
  | LogStr(_) => logged->Array.push("string")
  | LogRec(_) => ()
  }
  logged->Array.push("done")
}

describe(__MODULE__, () => {
  test("an empty arm keeps the checks before it", () => {
    logString(id(LogRec({x: 1})))
    logString(id(LogStr("a")))
    eq(__LOC__, ["done", "string", "done"], logged)
  })

  test("an instance handled by the default is not an object case", () => {
    eq(__LOC__, "other", objectOrString(id(D(Date.make()))))
    eq(__LOC__, "other", objectOrString(id(R(/a/))))
    eq(__LOC__, "O", objectOrString(id(O({x: 1}))))
    eq(__LOC__, "S", objectOrString(id(S("s"))))
    eq(__LOC__, "other", objectOrString(id(I(1))))
  })

  test("a literal handled by the default is not a string case", () => {
    eq(__LOC__, "other", textOrRecord(id(Lit)))
    eq(__LOC__, "text", textOrRecord(id(Text("lit2"))))
    eq(__LOC__, "record", textOrRecord(id(Rec({x: 1}))))
    eq(__LOC__, "other", textOrRecord(id(Count(1))))
  })

  test("primitives skip class checks in the two-arm form", () => {
    eq(__LOC__, "name", twoArmsKind(id(Name("a"))))
  })

  test("a value no arm of an incomplete match takes falls through", () => {
    eq(__LOC__, "number, any", pair(id(JSON.Number(1.)), id(JSON.Array([]))))
    eq(__LOC__, "number, any", pair(id(JSON.Number(1.)), id(JSON.Null)))
    eq(__LOC__, "any, object", pair(id(JSON.Number(1.)), id(JSON.Object(dict{}))))
    eq(__LOC__, "other", pair(id(JSON.String("a")), id(JSON.Array([]))))
  })

  test("literals skip class checks", () => {
    eq(__LOC__, "one", withLiteralsKind(id(One)))
    eq(__LOC__, "name", withLiteralsKind(id(NameLit("a"))))
  })
})
