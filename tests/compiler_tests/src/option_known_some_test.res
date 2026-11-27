open Mocha
open Test_utils

// Matching on [Some(x)] with [x] a variable reads [x] directly: no
// Primitive_option.some / valFromOption calls in the output.
let nested = (y: option<int>) =>
  switch Some(y) {
  | Some(Some(n)) => n
  | Some(None) => 0
  | None => -1
  }

let poly = (y: 'a) =>
  switch Some(y) {
  | Some(v) => [v]
  | None => []
  }

// A payload with side effects is evaluated once.
let calls = ref(0)

let next = () => {
  calls := calls.contents + 1
  calls.contents > 1 ? Some(calls.contents) : None
}

let effectful = () =>
  switch Some(next()) {
  | Some(Some(n)) => n
  | Some(None) => 0
  | None => -1
  }

describe(__MODULE__, () => {
  test("nested payload", () => {
    eq(__LOC__, 0, nested(None))
    eq(__LOC__, 3, nested(Some(3)))
  })
  test("polymorphic payload", () => {
    eq(__LOC__, [None], poly((None: option<int>)))
    eq(__LOC__, [Some(None)], poly((Some(None): option<option<int>>)))
    eq(__LOC__, [1], poly(1))
  })
  test("payload with side effects", () => {
    eq(__LOC__, 0, effectful())
    eq(__LOC__, 1, calls.contents)
    eq(__LOC__, 2, effectful())
    eq(__LOC__, 2, calls.contents)
  })
})
