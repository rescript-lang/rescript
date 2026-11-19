module type Ord = {
  type t
}

module IntOrd = {
  type t = int
}

module MakeSet = (O: Ord) => {
  type t = array<O.t>
  let add = (x: O.t) => [x]
}

module AliasSet = MakeSet

module Base = {
  type base = int
}
