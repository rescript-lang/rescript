// Test for type re-export: type y = x = {...}
// When fields are used through either type, both types' fields should be marked as used
// (via bidirectional linking between corresponding fields)
//
// Note: With type equations (type y = x = {...}), the type checker resolves field access
// to the re-exporting type's labels. The bidirectional linking ensures both types' fields
// are correctly marked as live/dead together regardless of which type is used in annotations.

// Test 1: Use fields through the re-exported type
// Expected: originalType.usedField = LIVE (propagated)
//           originalType.unusedField = DEAD
//           reexportedType.usedField = LIVE (external ref)
//           reexportedType.unusedField = DEAD
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
