// The constructors of an applied functor's type keep the stamps they have in
// the functor's file, here those of `v0` and `v1`.
module S = AppliedFunctorVariant.Make({
  type t = int
})

let v0 = 0
//  ^ref
let v1 = 1
//  ^ref
let v2 = 2
