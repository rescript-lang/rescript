type rec kind<'a> =
  | @as(0) IntKind: kind<int>
  | @as("x") StringKind: kind<string>

@unboxed
type t = Broad({kind: string, value: int}) | Narrow({kind: kind<int>, value: int})
