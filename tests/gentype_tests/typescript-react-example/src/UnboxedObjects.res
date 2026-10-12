@genType type createdKind = | @as("created") CreatedKind
@genType type stoppedKind = | @as("stopped") StoppedKind | @as("cancelled") CancelledKind

@genType type created = {kind: createdKind, id: int}
@genType type stopped = {kind: stoppedKind, reason: string}

@genType @unboxed type named = Created(created) | Stopped(stopped)

@genType @unboxed
type inline =
  | InlineCreated({kind: createdKind, id: int})
  | InlineStopped({kind: stoppedKind, reason: string})

@genType @unboxed
type mixed =
  | MixedCreated(created)
  | MixedStopped({kind: stoppedKind, reason: string})

@genType
let read = (value: named) =>
  switch value {
  | Created({id}) => id
  | Stopped({reason}) => String.length(reason)
  }

@genType @unboxed
type domains =
  | Text({kind: string, value: int})
  | Number({kind: float, amount: float})
  | Flag({kind: bool, enabled: bool})

@genType @unboxed
type precedence =
  | Literal({kind: createdKind, value: int})
  | Broad({kind: string, value: int})
