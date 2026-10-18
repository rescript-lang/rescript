// The guarded or-patterns below are unreachable. Compiling them used to fail
// with "Fatal error: Matching.comp_exit".
@@warning("-11")

open Mocha
open Test_utils

let g = () => true

let int = x =>
  switch x {
  | _ => 2
  | 1 | 2 if g() => 3
  }

let char = c =>
  switch c {
  | _ => "any"
  | 'a' | 'b' if g() => "ab"
  }

let string = s =>
  switch s {
  | _ => "any"
  | "a" | "b" if g() => "ab"
  }

let pair = p =>
  switch p {
  | (0, _) => "zero"
  | (_, _) => "other"
  | (1, 1) | (2, 2) if g() => "same"
  }

describe(__MODULE__, () => {
  test("unreachable guarded or-patterns", () => {
    eq(__LOC__, int(1), 2)
    eq(__LOC__, char('a'), "any")
    eq(__LOC__, string("a"), "any")
    eq(__LOC__, pair((0, 1)), "zero")
    eq(__LOC__, pair((1, 1)), "other")
  })
})
