type leftKind
type rightKind
type leftTag = LeftTag
type rightTag = RightTag
type left = {a: leftKind, b: leftTag}
type right = {a: rightKind, b: rightTag}
@unboxed type t = Left(left) | Right(right)
