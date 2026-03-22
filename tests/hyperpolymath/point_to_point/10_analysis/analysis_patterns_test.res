// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Analysis/LSP — patterns that analysis tools depend on
// Tests type annotation correctness, module resolution, and diagnostic patterns
// that the LSP analysis layer relies on.

let eq = (a, b) => a == b

// ─── Type annotation correctness ────────────────────────────────────
// Explicit annotations must be respected
let explicitInt: int = 42
Test.run(__POS_OF__("analysis: explicit int"), explicitInt, eq, 42)

let explicitFn: (int, int) => int = (a, b) => a + b
Test.run(__POS_OF__("analysis: explicit fn"), explicitFn(3, 4), eq, 7)

let explicitPoly: option<string> = Some("test")
Test.run(__POS_OF__("analysis: explicit poly"), explicitPoly, eq, Some("test"))

// ─── Module resolution patterns ─────────────────────────────────────
// Direct module access
module Resolv = {
  let x = 1
  module Sub = {
    let y = 2
    module Deep = {
      let z = 3
    }
  }
}
Test.run(__POS_OF__("analysis: module resolve"), Resolv.x, eq, 1)
Test.run(__POS_OF__("analysis: sub resolve"), Resolv.Sub.y, eq, 2)
Test.run(__POS_OF__("analysis: deep resolve"), Resolv.Sub.Deep.z, eq, 3)

// Module open and local open
let localOpen = {
  open Resolv
  x + Sub.y + Sub.Deep.z
}
Test.run(__POS_OF__("analysis: local open"), localOpen, eq, 6)

// ─── Type constraint patterns ───────────────────────────────────────
// Type constraints that analysis must track
let constrained = (x: int): string => Int.toString(x)
Test.run(__POS_OF__("analysis: constraint"), constrained(42), eq, "42")

// Polymorphic constraint
let polyConstrained: 'a => 'a = x => x
Test.run(__POS_OF__("analysis: poly constraint"), polyConstrained(42), eq, 42)

// ─── Signature patterns for analysis ────────────────────────────────
module type Signable = {
  type t
  let make: int => t
  let get: t => int
  let toString: t => string
}

module Signed: Signable = {
  type t = int
  let make = x => x
  let get = x => x
  let toString = Int.toString
}

Test.run(__POS_OF__("analysis: signature"), Signed.get(Signed.make(42)), eq, 42)
Test.run(__POS_OF__("analysis: sig toString"), Signed.toString(Signed.make(42)), eq, "42")

// ─── Hover info patterns ───────────────────────────────────────────
// Complex types that hover should display correctly
type complex<'a, 'b> = {
  value: 'a,
  transform: 'a => 'b,
  fallback: 'b,
}

let c: complex<int, string> = {
  value: 42,
  transform: Int.toString,
  fallback: "default",
}
Test.run(__POS_OF__("analysis: hover complex"), c.transform(c.value), eq, "42")

// ─── Completion-related patterns ────────────────────────────────────
// Chained method calls (dot completion)
let completionChain =
  [1, 2, 3]
  ->Array.map(x => x * 2)
  ->Array.filter(x => x > 2)
  ->Array.reduce(0, (a, b) => a + b)
Test.run(__POS_OF__("analysis: completion chain"), completionChain, eq, 10)

// Record field completion
type config = {host: string, port: int, debug: bool}
let cfg = {host: "localhost", port: 8080, debug: true}
Test.run(__POS_OF__("analysis: record field"), cfg.host, eq, "localhost")
Test.run(__POS_OF__("analysis: record field2"), cfg.port, eq, 8080)

// ─── Reference tracking patterns ────────────────────────────────────
// Multiple references to the same value
let sharedVal = 42
let ref1 = sharedVal
let ref2 = sharedVal + 1
let ref3 = sharedVal * 2
Test.run(__POS_OF__("analysis: ref tracking 1"), ref1, eq, 42)
Test.run(__POS_OF__("analysis: ref tracking 2"), ref2, eq, 43)
Test.run(__POS_OF__("analysis: ref tracking 3"), ref3, eq, 84)

// ─── Diagnostic patterns ───────────────────────────────────────────
// Patterns that commonly trigger diagnostics (all valid here)
// Exhaustive pattern match
type result2 = Ok2(int) | Err2(string) | Pending2
let handleResult = r =>
  switch r {
  | Ok2(v) => `ok:${Int.toString(v)}`
  | Err2(e) => `err:${e}`
  | Pending2 => "pending"
  }
Test.run(__POS_OF__("analysis: exhaustive ok"), handleResult(Ok2(1)), eq, "ok:1")
Test.run(__POS_OF__("analysis: exhaustive err"), handleResult(Err2("x")), eq, "err:x")
Test.run(__POS_OF__("analysis: exhaustive pending"), handleResult(Pending2), eq, "pending")

// ─── Decorator patterns for analysis ────────────────────────────────
@deprecated("Use newFn instead")
let oldFn = x => x + 1

// Using deprecated function still works
let _ = oldFn(5)
Test.run(__POS_OF__("analysis: deprecated works"), oldFn(5), eq, 6)

// ─── Rename/refactor patterns ───────────────────────────────────────
// Consistent naming across module boundary
module Renaming = {
  let targetName = 42
  let usesTarget = targetName + 1
}
let externalUse = Renaming.targetName + Renaming.usesTarget
Test.run(__POS_OF__("analysis: rename consistent"), externalUse, eq, 85)
