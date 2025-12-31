// First-class modules and functors test (interface)

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

module MakeShowable: (
  M: {
    type t
    let show: t => string
  },
) => (Showable with type t = M.t)

module MakeComparable: (
  M: {
    type t
    let compare: (t, t) => int
  },
) => (Comparable with type t = M.t)

// ==========================================
// Functors (multiple parameters)
// ==========================================

module MakePair: (
  Left: {
    type t
    let show: t => string
  },
  Right: {
    type t
    let show: t => string
  },
) =>
{
  type left = Left.t
  type right = Right.t
  let showPair: (left, right) => string
}

module MakeTriple: (
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
) =>
{
  type a = A.t
  type b = B.t
  type c = C.t
  let showTriple: (a, b, c) => string
}

// ==========================================
// Functions returning first-class modules
// ==========================================

let showable: (~show: 'a => string) => module(Showable with type t = 'a)

let comparable: (~compare: ('a, 'a) => int) => module(Comparable with type t = 'a)

let hashable: (~hash: 'a => int, ~eq: ('a, 'a) => bool) => module(Hashable with type t = 'a)

// ==========================================
// Using first-class modules
// ==========================================

let stringShowable: module(Showable with type t = string)

let intComparable: module(Comparable with type t = int)

let useShowable: (module(Showable with type t = 'a), 'a) => string
