// Inside a functor applied several times, completion and hover see the
// body's declarations, not the applications' copies.

module type Ord = {
  type t
  let compare: (t, t) => int
}

module IntOrd = {
  type t = int
  let compare = (a: int, b) => a - b
}

module StrOrd = {
  type t = string
  let compare = (a: string, b) => a < b ? -1 : a > b ? 1 : 0
}

module MakeSet = (O: Ord) => {
  type t = array<O.t>
  type shape = Dot | Box(O.t)
  let add = (s: t, x: O.t) => s->Array.concat([x])
  //   ^hov
  module Nested = {
    let nestedValue = (x: O.t) => x
  }
  // let z = ad
  //           ^com
  // let z = Nested.
  //                ^com
  // let z = Bo
  //           ^com
}

module A = MakeSet(IntOrd)
module B = MakeSet(StrOrd)
module C = MakeSet(IntOrd)

let a = A.add([], 1)
//        ^ref
