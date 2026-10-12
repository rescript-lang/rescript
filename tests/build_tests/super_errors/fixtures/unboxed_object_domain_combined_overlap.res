@unboxed type first = Text(string) | Number(int)
@unboxed type second = Flag(bool) | Decimal(float)
@unboxed type t = First({kind: first, value: int}) | Second({kind: second, reason: string})
