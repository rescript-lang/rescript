include IncludeDeclarationsLib.Make({
  type t = int
})

// Dot and Box keep their stamps from IncludeDeclarationsLib, which are the
// stamps of v0 and v1 here.
let v0 = 0
//  ^ref
let v1 = 1
//  ^ref
let v2 = 2

module type T = {
  type t
}
module F = (X: T) => {
  let fv = 1
  //  ^ref
}
include F({
  type t = int
})
let u = fv
//      ^ref

let dot = Dot
//        ^ref
let box = Box(1)
//        ^def
include IncludeDeclarationsLib.MakeMore({
  type t = int
})
let lv = {lf: 1}
//        ^ref
let m = made
//      ^ref

module G = (X: T) => {
  type local = L1 | L2
  type r = {rf: int}
}
include G({
  type t = int
})
let l = L1
//      ^ref
let l2 = L2
//       ^def
let rv = {rf: 1}
//        ^ref

module Base = {
  let bv = 1
  //  ^ref
  module Inner = {
    let deep = 1
  }
}
module Inc = {
  include Base
}
include Base
let b = Inc.bv + bv
let deep = Inner.deep + Inc.Inner.deep
//         ^ref

// Nested members of an included module copy the nested members, not the
// members of the same name one level up.
module H = (X: T) => {
  let x = 1
  module Nested = {
    let x = 2
    type t = B
    module Deep = {
      let x = 3
    }
  }
}
include H({
  type t = int
})
let hx = x
//       ^ref
let hNested = Nested.x
//                   ^ref
let hB = Nested.B
//              ^ref
let hDeep = Nested.Deep.x
//                      ^ref

// Several copies of one member, one of them a copy of a copy.
module Copy1 = {
  include Base
}
module Copy2 = {
  include Copy1
}
let copies = Copy1.bv + Copy2.bv
//                            ^ref

// A type read from a module type points at the module type's declaration.
module type WithType = {
  type wt
}
module WithTypeImpl: WithType = {
  type wt = int
}
let withType = (x: WithTypeImpl.wt) => x
//                              ^ref
