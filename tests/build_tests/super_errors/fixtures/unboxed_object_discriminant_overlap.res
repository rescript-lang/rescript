type ak = | @as("same") AKind
type bk = | @as("same") BKind
type a = {kind: ak}
type b = {kind: bk}
@unboxed type t = A(a) | B(b)
