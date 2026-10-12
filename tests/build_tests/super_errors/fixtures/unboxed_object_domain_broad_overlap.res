type a = {kind: string, value: int}
type b = {kind: string, value: int}
@unboxed type t = A(a) | B(b)
