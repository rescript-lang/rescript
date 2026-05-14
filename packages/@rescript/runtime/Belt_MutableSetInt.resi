/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
This module is [`Belt.MutableSet`]() specialized with key type to be a primitive type.

It is more efficient in general, the  API is the same with [`Belt.MutableSet`]() except its key type is fixed,
and identity is not needed(using the built-in one)

**See** [`Belt.MutableSet`]()
*/

/** The type of the set elements. */
type value = int

/** The type of sets. */
type t

let make: unit => t

let fromArray: array<value> => t
let fromSortedArrayUnsafe: array<value> => t

let copy: t => t
let isEmpty: t => bool
let has: (t, value) => bool

let add: (t, value) => unit
let addCheck: (t, value) => bool
let mergeMany: (t, array<value>) => unit
let remove: (t, value) => unit
let removeCheck: (t, value) => bool
let removeMany: (t, array<value>) => unit

let union: (t, t) => t
let intersect: (t, t) => t
let diff: (t, t) => t
let subset: (t, t) => bool

let cmp: (t, t) => int
let eq: (t, t) => bool

@deprecated("Use `forEach` instead")
let forEachU: (t, value => unit) => unit

/** In increasing order*/
let forEach: (t, value => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t, 'a, ('a, value) => 'a) => 'a

/** Iterate in increasing order. */
let reduce: (t, 'a, ('a, value) => 'a) => 'a

@deprecated("Use `every` instead")
let everyU: (t, value => bool) => bool

/**
`every(p, s)` checks if all elements of the set satisfy the predicate `p`.
Order unspecified. */
let every: (t, value => bool) => bool

@deprecated("Use `some` instead")
let someU: (t, value => bool) => bool

/**
`some(p, s)` checks if at least one element of the set satisfies the predicate
`p`. Oder unspecified.
*/
let some: (t, value => bool) => bool

@deprecated("Use `keep` instead")
let keepU: (t, value => bool) => t

/** 
`keep(s, p)` returns a fresh copy of the set of all elements in `s` that satisfy
predicate `p`.
*/
let keep: (t, value => bool) => t

@deprecated("Use `partition` instead")
let partitionU: (t, value => bool) => (t, t)

/** 
`partition(s, p)` returns a fresh copy pair of sets `(s1, s2)`, where `s1` is
the set of all the elements of `s` that satisfy the predicate `p`, and `s2` is
the set of all the elements of `s` that do not satisfy `p`. 
*/
let partition: (t, value => bool) => (t, t)

let size: t => int

/** In increasing order with respect */
let toList: t => list<value>

/** In increasing order with respect */
let toArray: t => array<value>

let minimum: t => option<value>
let maximum: t => option<value>

let get: (t, value) => option<value>
let getExn: (t, value) => value
let getOrThrow: (t, value) => value

/**
`split(s, key)` return a fresh copy of each
*/
let split: (t, value) => ((t, t), bool)

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t => unit
