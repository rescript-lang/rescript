// The members of an applied functor keep the doc comments of the functor's
// declarations.
module type Ord = {
  type t
}

module IntOrd = {
  type t = int
}

module MakeSet = (O: Ord) => {
  /** Adds an element. */
  let add = (s: array<O.t>, x: O.t) => s->Array.concat([x])
}

module IntSet = MakeSet(IntOrd)

let s = IntSet.add([], 1)
//              ^hov

// IntSet.a
//         ^com
