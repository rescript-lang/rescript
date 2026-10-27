module type Ord = {
  type t
  let compare: (t, t) => int
}

module MakeSet = (O: Ord) => {
  //     ^hov
  type t = array<O.t>
  let add = (s: t, x: O.t) => s->Array.concat([x])
}

module Pair = (A: Ord, B: Ord) => {
  //   ^hov
  type t = (A.t, B.t)
}

module Inline = (X: {
  //     ^hov
  let x: int
}) => {
  let y = X.x
}

module IntOrd = {
  type t = int
  let compare = (a: int, b) => a - b
}

// An applied functor is a plain module
module IntSet = MakeSet(IntOrd)
//      ^hov

// A partially applied functor keeps its remaining parameter
module PairInt = Pair(IntOrd)
//      ^hov

module Counter = () => {
  //     ^hov
  let count = ref(0)
}
