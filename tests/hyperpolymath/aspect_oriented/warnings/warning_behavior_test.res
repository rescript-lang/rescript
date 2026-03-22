// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Warning system behavior
// Cross-cutting concern: code that generates warnings must still produce
// correct runtime values.

let eq = (a, b) => a == b

// ─── Partial match still works for matched cases ────────────────────
type color = Red | Green | Blue | Yellow

// This function doesn't handle all cases but must work for those it does
let primaryOnly = c =>
  switch c {
  | Red => "red"
  | Green => "green"
  | Blue => "blue"
  | _ => "other"
  }

Test.run(__POS_OF__("warn: partial match red"), primaryOnly(Red), eq, "red")
Test.run(__POS_OF__("warn: partial match green"), primaryOnly(Green), eq, "green")
Test.run(__POS_OF__("warn: partial match other"), primaryOnly(Yellow), eq, "other")

// ─── Polymorphic comparison behavior ────────────────────────────────
// Structural equality with polymorphic comparison
Test.run(__POS_OF__("warn: poly cmp int"), 1 == 1, eq, true)
Test.run(__POS_OF__("warn: poly cmp string"), "a" == "a", eq, true)
Test.run(__POS_OF__("warn: poly cmp array"), [1, 2] == [1, 2], eq, true)
Test.run(__POS_OF__("warn: poly cmp record"), {contents: 1} == {contents: 1}, eq, true)

// ─── Open shadows behavior ─────────────────────────────────────────
module Shadowed = {
  let x = 42
  let y = "hello"
}

module Shadower = {
  let x = 99
}

let openShadowResult = {
  open Shadowed
  open Shadower // shadows Shadowed.x
  x + String.length(y)
}
Test.run(__POS_OF__("warn: open shadows"), openShadowResult, eq, 104)

// ─── Unused function arguments still compile ────────────────────────
let ignoredArg = (_unused1, _unused2, used) => used * 2
Test.run(__POS_OF__("warn: unused args"), ignoredArg(1, 2, 5), eq, 10)

// ─── Redundant pattern still compiles ───────────────────────────────
let redundant = x =>
  switch x {
  | 0 => "zero"
  | 1 => "one"
  | _ => "other"
  }
Test.run(__POS_OF__("warn: redundant zero"), redundant(0), eq, "zero")
Test.run(__POS_OF__("warn: redundant other"), redundant(99), eq, "other")

// ─── Wildcard catch-all ─────────────────────────────────────────────
exception WarnExn1
exception WarnExn2

let catchAll = f =>
  try {
    f()
    "ok"
  } catch {
  | _ => "caught"
  }

Test.run(__POS_OF__("warn: catch-all 1"), catchAll(() => raise(WarnExn1)), eq, "caught")
Test.run(__POS_OF__("warn: catch-all 2"), catchAll(() => raise(WarnExn2)), eq, "caught")
Test.run(__POS_OF__("warn: catch-all ok"), catchAll(() => ()), eq, "ok")

// ─── Type coercion with potential warnings ──────────────────────────
type animal = Cat2 | Dog2
type pet = Cat2 | Dog2 | Fish2

// Variant coercion
let animalToPet = (a: animal): pet =>
  switch a {
  | Cat2 => Cat2
  | Dog2 => Dog2
  }

Test.run(
  __POS_OF__("warn: variant coercion"),
  switch animalToPet(Cat2) {
  | Cat2 => "cat"
  | Dog2 => "dog"
  | Fish2 => "fish"
  },
  eq,
  "cat",
)

// ─── Nested wildcard patterns ──────────────────────────────────────
type nested = {a: int, b: option<int>}

let extractA = ({a, b: _}) => a
Test.run(__POS_OF__("warn: nested wildcard"), extractA({a: 42, b: Some(1)}), eq, 42)

// ─── Conditional with side effect in condition ──────────────────────
let condSideEffect = ref(false)
if {
  condSideEffect := true
  true
} {
  ()
}
Test.run(__POS_OF__("warn: cond side effect"), condSideEffect.contents, eq, true)

// ─── Sequence with ignored results ─────────────────────────────────
let seqResult = {
  ignore("ignored string")
  ignore(42)
  ignore([1, 2, 3])
  "final"
}
Test.run(__POS_OF__("warn: ignored results"), seqResult, eq, "final")
