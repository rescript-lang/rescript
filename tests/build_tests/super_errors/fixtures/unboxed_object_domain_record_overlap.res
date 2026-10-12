type a = {a: int}
type b = {b: string}
@unboxed type t = First({kind: a, value: int}) | Second({kind: b, reason: string})
