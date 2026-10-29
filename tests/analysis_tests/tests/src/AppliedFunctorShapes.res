module type Ord = {
  type t
  let compare: (t, t) => int
}

module IntOrd = {
  type t = int
  let compare = (a: int, b) => a - b
}

module Inline = ((O: Ord) => {
  let cmp = O.compare
  //          ^def
  let one = 1
})(IntOrd)

let one = Inline.one
//               ^def

module InlineCurried = ((A: Ord, B: Ord) => {
  let both = (A.compare, B.compare)
  //                       ^hov
})(IntOrd, IntOrd)

let both = InlineCurried.both
//                       ^ref

module Generative = (() => {
  let fresh = 1
})()

let fresh = Generative.fresh
//                     ^def

module MakeTwo = (A: Ord, B: Ord) => {
  let pair = (A.compare, B.compare)
}

module Two = MakeTwo(IntOrd, IntOrd)

let pair = Two.pair
//             ^ref

module Constrained = ((O: Ord) => {
  let c = O.compare
  //        ^def
}: (O: Ord) => {
  let c: (O.t, O.t) => int
})(IntOrd)

let c = Constrained.c
//                  ^def

module ToIdent = (O: Ord) => IntOrd

module Ident = ToIdent(IntOrd)

let ic = Ident.compare
//             ^def

module Nest = MakeTwo(ToIdent(IntOrd), IntOrd)

let np = Nest.pair
//            ^def
