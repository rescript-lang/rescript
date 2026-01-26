// Cross-file test: re-export happens in this file, manifest lives in the other file.

type reexportedRecord = TypeReexportCrossFileA.originalRecord = {
  usedField: string,
  unusedField: int, // dead (should not be reported on re-export)
}

let recordValue: reexportedRecord = {
  usedField: "x",
  unusedField: 1,
}

let _ = recordValue.usedField

