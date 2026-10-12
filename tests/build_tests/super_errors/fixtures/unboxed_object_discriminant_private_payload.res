module Payload: {
  type ak = AKind
  type a = private {kind: ak}
} = {
  type ak = AKind
  type a = {kind: ak}
}
type bk = BKind
type b = {kind: bk}
@unboxed type t = A(Payload.a) | B(b)
