module Pos = {
  type t = sourceLocPos

  type decoded = {
    file: string,
    startLine: int,
    startCol: int,
    endLine: int,
    endCol: int,
  }

  external toString: t => string = "%identity"

  let decode = (value: t): option<decoded> => {
    switch value->toString->Stdlib_String.split(";") {
    | [file, startLine, startCol, endLine, endCol] =>
      switch (
        startLine->Stdlib_Int.fromString,
        startCol->Stdlib_Int.fromString,
        endLine->Stdlib_Int.fromString,
        endCol->Stdlib_Int.fromString,
      ) {
      | (Some(startLine), Some(startCol), Some(endLine), Some(endCol)) =>
        Some({file, startLine, startCol, endLine, endCol})
      | _ => None
      }
    | _ => None
    }
  }
}

module ValuePath = {
  type t = sourceLocValuePath

  external toString: t => string = "%identity"

  let segments = (value: t) => value->toString->Stdlib_String.split(".")

  let name = (value: t) => {
    let segments = value->segments
    let length = segments->Stdlib_Array.length
    length === 0 ? "" : segments->Stdlib_Array.getUnsafe(length - 1)
  }
}
