/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
[`Belt.Id`]()

Provide utiliites to create identified comparators or hashes for
data structures used below.

It create a unique identifer per module of functions so that different data structures with slightly different
comparison functions won't mix.
*/

/**
`('a, 'id) hash`

Its runtime represenation is a `hash` function, but signed with a
type parameter, so that different hash functions type mismatch
*/
type hash<'a, 'id>

/**
`('a, 'id) eq`

Its runtime represenation is an `eq` function, but signed with a
type parameter, so that different hash functions type mismatch
*/
type eq<'a, 'id>

/**
`('a,'id) cmp`

Its runtime representation is a `cmp` function, but signed with a
type parameter, so that different hash functions type mismatch
*/
type cmp<'a, 'id>

module type Comparable = {
  type identity
  type t
  let cmp: cmp<t, identity>
}

/**
`('key, 'id) cmparable` is a module of functions, here it only includes `cmp`.

Unlike normal functions, when created, it comes with a unique identity (guaranteed
by the type system).

It can be created using function [`comparableU`]() or [`comparable`]().

The idea of a unique identity when created is that it makes sure two sets would type
mismatch if they use different comparison function
*/
type comparable<'key, 'id> = module(Comparable with type t = 'key and type identity = 'id)

@deprecated("Use `MakeComparable` instead")
module MakeComparableU: (
  M: {
    type t
    let cmp: (t, t) => int
  },
) => (Comparable with type t = M.t)

module MakeComparable: (
  M: {
    type t
    let cmp: (t, t) => int
  },
) => (Comparable with type t = M.t)

@deprecated("Use `comparable` instead")
let comparableU: (~cmp: ('a, 'a) => int) => module(Comparable with type t = 'a)

let comparable: (~cmp: ('a, 'a) => int) => module(Comparable with type t = 'a)

module type Hashable = {
  type identity
  type t
  let hash: hash<t, identity>
  let eq: eq<t, identity>
}

/**
`('key, 'id) hashable` is a module of functions, here it only includes `hash`, `eq`.

Unlike normal functions, when created, it comes with a unique identity (guaranteed
by the type system).

It can be created using function [`hashableU`]() or [`hashable`]().

The idea of a unique identity when created is that it makes sure two hash sets would type
mismatch if they use different comparison function
*/
type hashable<'key, 'id> = module(Hashable with type t = 'key and type identity = 'id)

@deprecated("Use `MakeHashable` instead")
module MakeHashableU: (
  M: {
    type t
    let hash: t => int
    let eq: (t, t) => bool
  },
) => (Hashable with type t = M.t)

module MakeHashable: (
  M: {
    type t
    let hash: t => int
    let eq: (t, t) => bool
  },
) => (Hashable with type t = M.t)

@deprecated("Use `hashable` instead")
let hashableU: (~hash: 'a => int, ~eq: ('a, 'a) => bool) => module(Hashable with type t = 'a)

let hashable: (~hash: 'a => int, ~eq: ('a, 'a) => bool) => module(Hashable with type t = 'a)

external getHashInternal: hash<'a, 'id> => 'a => int = "%identity"
external getEqInternal: eq<'a, 'id> => ('a, 'a) => bool = "%identity"
external getCmpInternal: cmp<'a, 'id> => ('a, 'a) => int = "%identity"
