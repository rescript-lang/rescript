// Fields in another order are reported with the expected order, next to the
// fields that only one declaration has
module M: {
  type t = {a: int, b: int, c: int, d: int}
} = {
  type t = {c: int, a: int, b: int, e: int}
}
