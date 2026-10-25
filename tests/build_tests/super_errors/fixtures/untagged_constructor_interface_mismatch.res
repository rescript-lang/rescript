module M: {
  type t = | @unboxed A(int) | B({x: int})
} = {
  type t = A(int) | B({x: int})
}
