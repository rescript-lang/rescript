// Cross-file test: type re-export equations should link liveness across files.
// This file defines the original record type only.

type originalRecord = {
  usedField: string,
  unusedField: int, // dead (reported on original only)
}

