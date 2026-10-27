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
