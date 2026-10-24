open Mocha
open Test_utils

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

describe(__MODULE__, () => {
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
})
