type ak = AKind
type a<'kind> = {kind: 'kind}
type b = {kind: ak}
@unboxed type t<'kind> = A(a<'kind>) | B(b)
