type ak = AKind
type bk = BKind
type ck = CKind
type a = {a: ak, b: ak}
type b = {a: ak, b: bk}
type c = {a: ck, b: ck}
@unboxed type source = A(a) | C(c)
@unboxed type target = A(a) | B(b) | C(c)
let convert = (value: source) => (value :> target)
