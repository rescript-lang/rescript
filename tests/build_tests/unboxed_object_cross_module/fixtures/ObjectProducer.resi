type leftKind = | @as("left") LeftKind
@unboxed type rightKind = | @as("right") RightKind | @as("other") OtherKind
type left = {kind: leftKind, value: int}
@unboxed type t = Left(left) | Right({kind: rightKind, value: int})
let make: unit => t
let read: t => int
