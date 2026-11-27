open Mocha
open Test_utils

// Aliases of builtin types are recognized through their expansion
type entries = dict<int>
type label = string
type pair = (int, int)

@unboxed
type value = Entries(entries) | Label(label) | Pair(pair)

let kind = value =>
  switch value {
  | Entries(_) => "entries"
  | Label(label) => label
  | Pair(_) => "pair"
  }

// An annotation can restate what a builtin type is recognized by, and an
// interface can state it over a builtin implementation
@typeOf("number") type count = int

module Score: {
  @typeOf("number") type t
  let make: int => t
} = {
  type t = int
  let make = x => x
}

@unboxed
type counted = Count(count) | CountName(string)

let countedKind = value =>
  switch value {
  | Count(_) => "number"
  | CountName(name) => name
  }

@unboxed
type scored = Score(Score.t) | ScoreName(string)

let scoredKind = value =>
  switch value {
  | Score(_) => "number"
  | ScoreName(name) => name
  }

describe(__MODULE__, () => {
  test("aliases of builtin types", () => {
    eq(__LOC__, "entries", kind(Entries(dict{"a": 1})))
    eq(__LOC__, "text", kind(Label("text")))
    eq(__LOC__, "pair", kind(Pair((1, 2))))
  })

  test("annotations stating a builtin type's kind", () => {
    eq(__LOC__, "number", countedKind(Count(1)))
    eq(__LOC__, "name", countedKind(CountName("name")))
    eq(__LOC__, "number", scoredKind(Score(Score.make(2))))
    eq(__LOC__, "name", scoredKind(ScoreName("name")))
  })
})
