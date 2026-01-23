// Tests for type re-export equations: `type y = x = ...`
//
// This covers both:
// - record labels
// - variant constructors
//
// When labels/cases are used through either type, liveness should be linked
// bidirectionally between corresponding declarations.
//
// Reporting policy:
// - we suppress dead-type warnings for the *re-exported* copy of labels/cases
//   (they are restated but not independently actionable)
// - we still report on the original type's labels/cases
//
// Note: With type equations (type y = x = {...}), the type checker resolves field access
// to the re-exporting type's labels. The bidirectional linking ensures both types' fields
// are correctly marked as live/dead together regardless of which type is used in annotations.

// Test 1: Use fields through the re-exported type
// Expected: originalType.usedField = LIVE (propagated)
//           originalType.unusedField = DEAD
//           reexportedType.usedField = LIVE (external ref)
//           reexportedType.unusedField = DEAD
//           warnings: only originalType.unusedField should be reported
module UseReexported = {
  type originalType = {
    usedField: string,
    unusedField: int, // dead
  }

  type reexportedType = originalType = {
    usedField: string,
    unusedField: int, // dead
  }

  let value: reexportedType = {usedField: "test", unusedField: 42}
  let _ = value.usedField
}

// Test 2: Annotate with original type (linking still works)
// Expected: originalType.directlyUsed = LIVE (propagated)
//           originalType.alsoUnused = DEAD
//           reexportedType.directlyUsed = LIVE (external ref)
//           reexportedType.alsoUnused = DEAD
//           warnings: only originalType.alsoUnused should be reported
module UseOriginal = {
  type originalType = {
    directlyUsed: string,
    alsoUnused: int, // dead
  }

  type reexportedType = originalType = {
    directlyUsed: string,
    alsoUnused: int, // dead
  }

  let value: originalType = {directlyUsed: "direct", alsoUnused: 0}
  let _ = value.directlyUsed
}

// Test 3: Re-exported type defined but never explicitly used in annotations
// The bidirectional linking ensures both types' fields are marked consistently
// Expected: originalType.usedField = LIVE (propagated)
//           originalType.unusedField = DEAD
//           reexportedType.usedField = LIVE (external ref)
//           reexportedType.unusedField = DEAD
//           warnings: only originalType.unusedField should be reported
module OnlyReexportedDead = {
  type originalType = {
    usedField: string,
    unusedField: int, // dead
  }

  // Re-export - fields linked bidirectionally with originalType
  type reexportedType = originalType = {
    usedField: string,
    unusedField: int, // dead
  }

  let value: originalType = {usedField: "test", unusedField: 42}
  let _ = value.usedField
}

// Variant Test 1: Use constructor via the re-exported type annotation
// Expected: originalType.A = LIVE (propagated)
//           originalType.B = DEAD
//           reexportedType.A = LIVE (external ref)
//           reexportedType.B = DEAD
//           warnings: only originalType.B should be reported
module VariantUseReexported = {
  type originalType = A | B
  type reexportedType = originalType = A | B

  let value: reexportedType = A
  let _ =
    switch value {
    | A => ()
    | B => ()
    }
}

// Variant Test 2: Annotate with original type (linking still works)
// Expected: originalType.A = LIVE (propagated)
//           originalType.B = DEAD
//           reexportedType.A = LIVE (external ref)
//           reexportedType.B = DEAD
//           warnings: only originalType.B should be reported
module VariantUseOriginal = {
  type originalType = A | B
  type reexportedType = originalType = A | B

  let value: originalType = A
  let _ =
    switch value {
    | A => ()
    | B => ()
    }
}
