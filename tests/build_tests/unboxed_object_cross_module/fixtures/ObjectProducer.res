type leftKind = | @as("left") LeftKind
@unboxed type rightKind = | @as("right") RightKind | @as("other") OtherKind
type left = {kind: leftKind, value: int}
@unboxed type t = Left(left) | Right({kind: rightKind, value: int})
let make = () => Left({kind: LeftKind, value: 42})
let read = value =>
  switch value {
  | Left({value}) | Right({value}) => value
  }
