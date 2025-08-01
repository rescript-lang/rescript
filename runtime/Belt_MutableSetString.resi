/* Copyright (C) 2017 Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/***
This module is [`Belt.MutableSet`]() specialized with key type to be a primitive type.

It is more efficient in general, the  API is the same with [`Belt.MutableSet`]() except its key type is fixed,
and identity is not needed(using the built-in one)

**See** [`Belt.MutableSet`]()
*/

/** The type of the set elements. */
type value = string

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
let minUndefined: t => Js.undefined<value>
let maximum: t => option<value>
let maxUndefined: t => Js.undefined<value>

let get: (t, value) => option<value>
let getUndefined: (t, value) => Js.undefined<value>
let getExn: (t, value) => value
let getOrThrow: (t, value) => value

/**
`split(s, key)` return a fresh copy of each
*/
let split: (t, value) => ((t, t), bool)

/**
**raise** when invariant is not held
*/
let checkInvariantInternal: t => unit
