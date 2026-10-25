@typeOf("symbol") type a
@typeOf("symbol") type b

@unboxed
type t = A(a) | B(b)
