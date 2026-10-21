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

describe(__MODULE__, () => {
  test("aliases of builtin types", () => {
    eq(__LOC__, "entries", kind(Entries(dict{"a": 1})))
    eq(__LOC__, "text", kind(Label("text")))
    eq(__LOC__, "pair", kind(Pair((1, 2))))
  })
})
