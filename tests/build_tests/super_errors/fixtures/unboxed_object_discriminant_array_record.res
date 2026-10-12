type ak = AKind
type bk = BKind
type a = {@as("0") kind: ak}
type b = {kind: bk}
@unboxed type t = A(a) | B(b)
