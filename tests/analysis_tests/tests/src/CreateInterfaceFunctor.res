// ^int

module type Ord = {
  type t
  let compare: (t, t) => int
}

module MakeSet = (O: Ord) => {
  type t = array<O.t>
  let add = (s: t, x: O.t) => s->Array.concat([x])
}

module IntSet = MakeSet({
  type t = int
  let compare = (a: int, b) => a - b
})

// Types of the same file outside a functor keep their attributes
module M = {
  type t = {@as("wire") field: int}
}

module type S = module type of M

module N = {
  include M
}

// Types of an applied functor keep the runtime names of their fields
module WithWire = (X: {}) => {
  type t = {@as("wire") field: int}
}

module Applied = WithWire({})
