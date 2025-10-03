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

module Int = Belt_MutableMapInt

module String = Belt_MutableMapString

/***
A mutable sorted map module which allows customize compare behavior.

Same as `Belt.Map`, but mutable.
*/

/*
## Examples

```rescript
type t<'k, 'v, 'id>
type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
```
*/

type t<'k, 'v, 'id>
type id<'key, 'id> = Belt_Id.comparable<'key, 'id>

let make: (~id: id<'k, 'id>) => t<'k, 'a, 'id>
let clear: t<_> => unit
let isEmpty: t<_> => bool
let has: (t<'k, _, _>, 'k) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'k, 'a, 'id>, t<'k, 'a, 'id>, ('a, 'a) => int) => int
/**
`cmp(m1, m2, cmp)` First compare by size, if size is the same, compare by
key, value pair.
*/
let cmp: (t<'k, 'a, 'id>, t<'k, 'a, 'id>, ('a, 'a) => int) => int

@deprecated("Use `eq` instead")
let eqU: (t<'k, 'a, 'id>, t<'k, 'a, 'id>, ('a, 'a) => bool) => bool
/**
`eq(m1, m2, eqf)` tests whether the maps `m1` and `m2` are equal, that is,
contain equal keys and associate them with equal data. `eqf` is the
equality predicate used to compare the data associated with the keys.
*/
let eq: (t<'k, 'a, 'id>, t<'k, 'a, 'id>, ('a, 'a) => bool) => bool

@deprecated("Use `forEach` instead")
let forEachU: (t<'k, 'a, 'id>, ('k, 'a) => unit) => unit
/**
`forEach(m, f)` applies f to all bindings in map `m`. `f` receives the `'k`
as first argument, and the associated value as second argument. The
bindings are passed to `f` in increasing order with respect to the ordering
over the type of the keys.
*/
let forEach: (t<'k, 'a, 'id>, ('k, 'a) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'k, 'a, 'id>, 'b, ('b, 'k, 'a) => 'b) => 'b
/**
`reduce(m, a, f), computes`(f(kN, dN) ... (f(k1, d1, a))...)`, where`k1 ...
kN`are the keys of all bindings in`m`(in increasing order), and`d1 ... dN`
are the associated data.
*/
let reduce: (t<'k, 'a, 'id>, 'b, ('b, 'k, 'a) => 'b) => 'b

@deprecated("Use `every` instead")
let everyU: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool
/**
`every(m, p)` checks if all the bindings of the map satisfy the predicate `p`.
*/
let every: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool

@deprecated("Use `some` instead")
let someU: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool
/**
`some(m, p)` checks if at least one binding of the map satisfy the predicate `p`.
*/
let some: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool

let size: t<'k, 'a, 'id> => int

/** In increasing order. */
let toList: t<'k, 'a, 'id> => list<('k, 'a)>

let toArray: t<'k, 'a, 'id> => array<('k, 'a)>

let fromArray: (array<('k, 'a)>, ~id: id<'k, 'id>) => t<'k, 'a, 'id>
let keysToArray: t<'k, _, _> => array<'k>
let valuesToArray: t<_, 'a, _> => array<'a>
let minKey: t<'k, _, _> => option<'k>
let minKeyUndefined: t<'k, _, _> => Js.undefined<'k>
let maxKey: t<'k, _, _> => option<'k>
let maxKeyUndefined: t<'k, _, _> => Js.undefined<'k>
let minimum: t<'k, 'a, _> => option<('k, 'a)>
let minUndefined: t<'k, 'a, _> => Js.undefined<('k, 'a)>
let maximum: t<'k, 'a, _> => option<('k, 'a)>
let maxUndefined: t<'k, 'a, _> => Js.undefined<('k, 'a)>
let get: (t<'k, 'a, 'id>, 'k) => option<'a>
let getUndefined: (t<'k, 'a, 'id>, 'k) => Js.undefined<'a>
let getWithDefault: (t<'k, 'a, 'id>, 'k, 'a) => 'a
let getExn: (t<'k, 'a, 'id>, 'k) => 'a
let getOrThrow: (t<'k, 'a, 'id>, 'k) => 'a
/** Throw when invariant is not held. */
let checkInvariantInternal: t<_> => unit

/* ************************************************************************** */

/* TODO: add functional `merge, partition, keep, split` */

/** `remove(m, x)` do the in-place modification. */
let remove: (t<'k, 'a, 'id>, 'k) => unit

let removeMany: (t<'k, 'a, 'id>, array<'k>) => unit

/** `set(m, x, y)` do the in-place modification */
let set: (t<'k, 'a, 'id>, 'k, 'a) => unit

@deprecated("Use `update` instead")
let updateU: (t<'k, 'a, 'id>, 'k, option<'a> => option<'a>) => unit
let update: (t<'k, 'a, 'id>, 'k, option<'a> => option<'a>) => unit

let mergeMany: (t<'k, 'a, 'id>, array<('k, 'a)>) => unit

@deprecated("Use `map` instead")
let mapU: (t<'k, 'a, 'id>, 'a => 'b) => t<'k, 'b, 'id>
/**
`map(m, f)` returns a map with same domain as `m`, where the associated
value a of all bindings of `m` has been replaced by the result of the
application of `f` to `a`. The bindings are passed to `f` in increasing
order with respect to the ordering over the type of the keys.
*/
let map: (t<'k, 'a, 'id>, 'a => 'b) => t<'k, 'b, 'id>

@deprecated("Use `mapWithKey` instead")
let mapWithKeyU: (t<'k, 'a, 'id>, ('k, 'a) => 'b) => t<'k, 'b, 'id>
let mapWithKey: (t<'k, 'a, 'id>, ('k, 'a) => 'b) => t<'k, 'b, 'id>
