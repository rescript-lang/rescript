// A functor applied to a module path: the compiler names each type of the
// application by a path no source can write, such as `Make(IntOrd).box`.
module type Ord = {
  type t
}
module IntOrd = {
  type t = int
}
module Make = (O: Ord) => {
  type box = {v: O.t, mutable n: int}
  type kind = Leaf | Node(O.t)
  type abs
  /** doc of get */
  let get = (b: box) => b.v
  let mk = (v): box => {v, n: 0}
  let abs: abs = %raw(`1`)
  let useAbs = (a: abs) => 1
}
module B = Make(IntOrd)
let b = B.mk(1)
let v = B.get(b)
//        ^hov
// b->
//    ^com
// b.
//   ^com
let a = B.abs
// a->
//    ^com
let k = B.Leaf
//  ^hov
// let k: B.kind =
//                 ^com
// switch k { |
//             ^com
let f = (x: B.box) => x
// f(
//   ^she
// f({
//    ^com
