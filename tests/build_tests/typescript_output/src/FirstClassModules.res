// First-class modules and functors test

// ==========================================
// Module Types (become TypeScript interfaces)
// ==========================================

module type Showable = {
  type t
  let show: t => string
}

module type Comparable = {
  type identity
  type t
  let compare: (t, t) => int
}

module type Hashable = {
  type identity
  type t
  let hash: t => int
  let eq: (t, t) => bool
}

// ==========================================
// First-class module types (type aliases)
// ==========================================

type showable<'a> = module(Showable with type t = 'a)
type comparable<'key, 'id> = module(Comparable with type t = 'key and type identity = 'id)
type hashable<'key, 'id> = module(Hashable with type t = 'key and type identity = 'id)

// ==========================================
// Functors (single parameter)
// ==========================================

module MakeShowable = (
  M: {
    type t
    let show: t => string
  },
): (Showable with type t = M.t) => {
  type t = M.t
  let show = M.show
}

module MakeComparable = (
  M: {
    type t
    let compare: (t, t) => int
  },
): (Comparable with type t = M.t) => {
  type identity
  type t = M.t
  let compare = M.compare
}

// ==========================================
// Functors (multiple parameters)
// ==========================================

module MakePair = (
  Left: {
    type t
    let show: t => string
  },
  Right: {
    type t
    let show: t => string
  },
): {
  type left = Left.t
  type right = Right.t
  let showPair: (left, right) => string
} => {
  type left = Left.t
  type right = Right.t
  let showPair = (l, r) => "(" ++ Left.show(l) ++ ", " ++ Right.show(r) ++ ")"
}

module MakeTriple = (
  A: {
    type t
    let show: t => string
  },
  B: {
    type t
    let show: t => string
  },
  C: {
    type t
    let show: t => string
  },
): {
  type a = A.t
  type b = B.t
  type c = C.t
  let showTriple: (a, b, c) => string
} => {
  type a = A.t
  type b = B.t
  type c = C.t
  let showTriple = (x, y, z) => "(" ++ A.show(x) ++ ", " ++ B.show(y) ++ ", " ++ C.show(z) ++ ")"
}

// ==========================================
// Functions returning first-class modules
// ==========================================

let showable = (type a, ~show: a => string): module(Showable with type t = a) =>
  module(
    {
      type t = a
      let show = show
    }
  )

let comparable = (type a, ~compare: (a, a) => int): module(Comparable with type t = a) =>
  module(
    {
      type identity
      type t = a
      let compare = compare
    }
  )

let hashable = (type a, ~hash: a => int, ~eq: (a, a) => bool): module(Hashable with type t = a) =>
  module(
    {
      type identity
      type t = a
      let hash = hash
      let eq = eq
    }
  )

// ==========================================
// Using first-class modules
// ==========================================

let stringShowable = module(
  {
    type t = string
    let show = s => s
  }: Showable with type t = string
)

let intComparable = comparable(~compare=(a, b) => a - b)

// Pack and unpack
let useShowable = (type a, m: module(Showable with type t = a), x: a) => {
  module M = unpack(m)
  M.show(x)
}
