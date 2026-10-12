type ak = AKind
type bk = BKind
type a = {mutable kind: ak}
type b = {kind: bk}
@unboxed type t = A(a) | B(b)
