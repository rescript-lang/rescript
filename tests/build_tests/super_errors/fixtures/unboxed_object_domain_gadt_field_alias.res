type rec kind<'a> =
  | @as(0) IntKind: kind<int>
  | @as("x") StringKind: kind<string>

type integerKind = kind<int>
@unboxed type wrapped = Wrapped(integerKind)
@unboxed
type t = Broad({kind: string, value: int}) | Narrow({kind: wrapped, value: int})
