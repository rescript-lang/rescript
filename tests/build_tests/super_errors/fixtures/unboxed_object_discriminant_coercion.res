type ak = AKind
type bk = BKind
type a = {kind: ak}
type b = {kind: bk}
@unboxed type source = A(a)
@unboxed type target = A(a) | B(b)
let convert = (value: source) => (value :> target)
