open Mocha
open Test_utils

// @notUndefined on an abbreviation refines a type whose representation the
// compiler does not see, or restates that a type cannot be undefined

type opaque
@notUndefined type refined = opaque

@notUndefined type count = int

@unboxed
type value = Count(count) | Name(string)

let kind = value =>
  switch value {
  | Count(_) => "count"
  | Name(name) => name
  }

let wrap = (x: refined) => Some(x)

let unwrap = (x: option<refined>) =>
  switch x {
  | Some(_) => "some"
  | None => "none"
  }

let refinedValue: refined = %raw(`{}`)

describe(__MODULE__, () => {
  test("a refined abstract type is not boxed in an option", () => {
    eq(__LOC__, Obj.magic(wrap(refinedValue)) === refinedValue, true)
    eq(__LOC__, unwrap(wrap(refinedValue)), "some")
    eq(__LOC__, unwrap(None), "none")
  })

  test("a restated builtin keeps its payload kind", () => {
    eq(__LOC__, kind(Count(1)), "count")
    eq(__LOC__, kind(Name("name")), "name")
  })
})
