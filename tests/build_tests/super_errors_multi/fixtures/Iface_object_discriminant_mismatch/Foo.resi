type leftKind = LeftKind
type rightKind = RightKind
type left = {kind: leftKind}
type right = {kind: rightKind}
@unboxed type t = Left(left) | Right(right)
