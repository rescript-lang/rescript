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

type key = int

type t<'a>

let make: unit => t<'a>
let clear: t<'a> => unit
let isEmpty: t<'a> => bool

let has: (t<'a>, key) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'a>, t<'a>, ('a, 'a) => int) => int

/** 
`cmp(m1, m2, cmp)`. First compare by size, if size is the same, compare by key,
value pair
*/
let cmp: (t<'a>, t<'a>, ('a, 'a) => int) => int

@deprecated("Use `eq` instead")
let eqU: (t<'a>, t<'a>, ('a, 'a) => bool) => bool

/** `eq(m1, m2, cmp)` */
let eq: (t<'a>, t<'a>, ('a, 'a) => bool) => bool

@deprecated("Use `forEach` instead")
let forEachU: (t<'a>, (key, 'a) => unit) => unit

/** 
`forEach(m, f)` applies `f` to all bindings in map `m`. `f` receives the key as
first argument, and the associated value as second argument. The application 
order of `f` is in increasing order. */
let forEach: (t<'a>, (key, 'a) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'a>, 'b, ('b, key, 'a) => 'b) => 'b

/** 
`reduce(m, a, f)` computes `(f kN dN ... (f k1 d1 a)...)`, where `k1 ... kN` are
the keys of all bindings in `m` (in increasing order), and `d1 ... dN` are the
associated data. */
let reduce: (t<'a>, 'b, ('b, key, 'a) => 'b) => 'b

@deprecated("Use `every` instead")
let everyU: (t<'a>, (key, 'a) => bool) => bool

/** 
`every(m, p)` checks if all the bindings of the map satisfy the predicate `p`.
The application order of `p` is unspecified.
*/
let every: (t<'a>, (key, 'a) => bool) => bool

@deprecated("Use `some` instead")
let someU: (t<'a>, (key, 'a) => bool) => bool

/** 
`some(m, p)` checks if at least one binding of the map satisfy the predicate `p`.
The application order of `p` is unspecified.
*/
let some: (t<'a>, (key, 'a) => bool) => bool

let size: t<'a> => int

/** In increasing order */
let toList: t<'a> => list<(key, 'a)>

/** In increasing order */
let toArray: t<'a> => array<(key, 'a)>

let fromArray: array<(key, 'a)> => t<'a>
let keysToArray: t<'a> => array<key>
let valuesToArray: t<'a> => array<'a>
let minKey: t<_> => option<key>
let minKeyUndefined: t<_> => Js.undefined<key>
let maxKey: t<_> => option<key>
let maxKeyUndefined: t<_> => Js.undefined<key>
let minimum: t<'a> => option<(key, 'a)>
let minUndefined: t<'a> => Js.undefined<(key, 'a)>
let maximum: t<'a> => option<(key, 'a)>
let maxUndefined: t<'a> => Js.undefined<(key, 'a)>
let get: (t<'a>, key) => option<'a>
let getUndefined: (t<'a>, key) => Js.undefined<'a>
let getWithDefault: (t<'a>, key, 'a) => 'a
let getExn: (t<'a>, key) => 'a
let getOrThrow: (t<'a>, key) => 'a

/**
  **Throw** when invariant is not held
*/
let checkInvariantInternal: t<_> => unit

/* ************************************************************************** */

/* TODO: add functional `merge, partition, keep, split` */

/** `remove(m, x)` do the in-place modification */
let remove: (t<'a>, key) => unit

let removeMany: (t<'a>, array<key>) => unit

/** 
`set(m, x, y)` do the in-place modification, return `m` for chaining. If `x` was
already bound in `m`, its previous binding disappears.
*/
let set: (t<'a>, key, 'a) => unit

@deprecated("Use `update` instead")
let updateU: (t<'a>, key, option<'a> => option<'a>) => unit
let update: (t<'a>, key, option<'a> => option<'a>) => unit

@deprecated("Use `map` instead")
let mapU: (t<'a>, 'a => 'b) => t<'b>

/** 
`map(m, f)` returns a map with same domain as `m`, where the associated value `a`
of all bindings of `m` has been replaced by the result of the application of `f`
to `a`. The bindings are passed to `f` in increasing order with respect to the
ordering over the type of the keys. */
let map: (t<'a>, 'a => 'b) => t<'b>

@deprecated("Use `mapWithKey` instead")
let mapWithKeyU: (t<'a>, (key, 'a) => 'b) => t<'b>
let mapWithKey: (t<'a>, (key, 'a) => 'b) => t<'b>
