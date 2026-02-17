/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
This module is [`Belt.Set`]() specialized with value type to be a primitive type.
It is more efficient in general, the  API is the same with [`Belt_Set`]() except its value type is fixed,
and identity is not needed(using the built-in one)

**See** [`Belt.Set`]()
*/

/** The type of the set elements. */
type value = int

/** The type of sets. */
type t

let empty: t

let fromArray: array<value> => t

let fromSortedArrayUnsafe: array<value> => t

let isEmpty: t => bool

let has: (t, value) => bool

/**
`add(s, x)` If `x` was already in `s`, `s` is returned unchanged.
*/
let add: (t, value) => t

let mergeMany: (t, array<value>) => t

/**
`remove(m, x)` If `x` was not in `m`, `m` is returned reference unchanged.
*/
let remove: (t, value) => t

let removeMany: (t, array<value>) => t

let union: (t, t) => t

let intersect: (t, t) => t

let diff: (t, t) => t

/**
`subset(s1, s2)` tests whether the set `s1` is a subset of the set `s2`.
*/
let subset: (t, t) => bool

/**
Total ordering between sets. Can be used as the ordering function for doing sets
of sets.
*/
let cmp: (t, t) => int

/**
`eq(s1, s2)` tests whether the sets `s1` and `s2` are equal, that is, contain
equal elements.
*/
let eq: (t, t) => bool

@deprecated("Use `forEach` instead")
let forEachU: (t, value => unit) => unit

/**
`forEach(s, f)` applies `f` in turn to all elements of `s`. In increasing order
*/
let forEach: (t, value => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t, 'a, ('a, value) => 'a) => 'a

/** Iterate in increasing order. */
let reduce: (t, 'a, ('a, value) => 'a) => 'a

@deprecated("Use `every` instead")
let everyU: (t, value => bool) => bool

/**
`every(p, s)` checks if all elements of the set satisfy the predicate `p`. Order
unspecified.
*/
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
`keep(p, s)` returns the set of all elements in `s` that satisfy predicate `p`.
*/
let keep: (t, value => bool) => t

@deprecated("Use `partition` instead")
let partitionU: (t, value => bool) => (t, t)

/**
`partition(p, s)` returns a pair of sets `(s1, s2)`, where `s1` is the set of
all the elements of `s` that satisfy the predicate `p`, and `s2` is the set of
all the elements of `s` that do not satisfy `p`.
*/
let partition: (t, value => bool) => (t, t)

let size: t => int

/** In increasing order */
let toList: t => list<value>

let toArray: t => array<value>

let minimum: t => option<value>

let minUndefined: t => Js.undefined<value>

let maximum: t => option<value>

let maxUndefined: t => Js.undefined<value>

let get: (t, value) => option<value>

let getUndefined: (t, value) => Js.undefined<value>

let getExn: (t, value) => value

let getOrThrow: (t, value) => value

/**
`split(x, s)` returns a triple `(l, present, r)`, where `l` is the set of
elements of `s` that are strictly less than `x`;`r` is the set of elements of
`s` that are strictly greater than `x`; `present` is `false` if `s` contains no
element equal to `x`, or `true` if `s` contains an element equal to `x`.
*/
let split: (t, value) => ((t, t), bool)

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t => unit
