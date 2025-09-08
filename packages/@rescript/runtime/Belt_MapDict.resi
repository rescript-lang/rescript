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
This module separates identity from data, it is a bit more verbose but
slightly more efficient due to the fact that there is no need to pack
identity and data back after each operation.

**_Advanced usage only_**
*/

/*
## Examples

```rescript
type t<'key, 'value, 'id>
type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>
```
*/

type t<'key, 'value, 'id>

type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

let empty: t<'k, 'v, 'id>

let isEmpty: t<'k, 'v, 'id> => bool

let has: (t<'k, 'a, 'id>, 'k, ~cmp: cmp<'k, 'id>) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'k, 'v, 'id>, t<'k, 'v, 'id>, ~kcmp: cmp<'k, 'id>, ~vcmp: ('v, 'v) => int) => int
let cmp: (t<'k, 'v, 'id>, t<'k, 'v, 'id>, ~kcmp: cmp<'k, 'id>, ~vcmp: ('v, 'v) => int) => int

@deprecated("Use `eq` instead")
let eqU: (t<'k, 'a, 'id>, t<'k, 'a, 'id>, ~kcmp: cmp<'k, 'id>, ~veq: ('a, 'a) => bool) => bool
/**
`eq(m1, m2, cmp)` tests whether the maps `m1` and `m2` are equal, that is,
contain equal keys and associate them with equal data. `cmp` is the
equality predicate used to compare the data associated with the keys. 
*/
let eq: (t<'k, 'a, 'id>, t<'k, 'a, 'id>, ~kcmp: cmp<'k, 'id>, ~veq: ('a, 'a) => bool) => bool

@deprecated("Use `findFirstBy` instead")
let findFirstByU: (t<'k, 'v, 'id>, ('k, 'v) => bool) => option<('k, 'v)>
/** 
`findFirstBy(m, p)` uses function `f` to find the first key value pair to
match predicate `p`.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Map.Dict.fromArray([(4, "4"), (1, "1"), (2, "2"), (3, "3")], ~cmp=IntCmp.cmp)

Belt.Map.Dict.findFirstBy(s0, (k, _) => k == 4) == Some((4, "4"))
```
*/
let findFirstBy: (t<'k, 'v, 'id>, ('k, 'v) => bool) => option<('k, 'v)>

@deprecated("Use `forEach` instead")
let forEachU: (t<'k, 'a, 'id>, ('k, 'a) => unit) => unit
/**
`forEach(m, f)` applies `f` to all bindings in map `m`. `f` receives the
key as first argument, and the associated value as second argument. The
bindings are passed to `f` in increasing order with respect to the ordering
over the type of the keys.
*/
let forEach: (t<'k, 'a, 'id>, ('k, 'a) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'k, 'a, 'id>, 'b, ('b, 'k, 'a) => 'b) => 'b
/**
`reduce(m, a, f)` computes `f(kN, dN ... f(k1, d1, a)...)`, where `k1 ... kN` 
are the keys of all bindings in `m` (in increasing order), and `d1 ... dN` 
are the associated data.
*/
let reduce: (t<'k, 'a, 'id>, 'b, ('b, 'k, 'a) => 'b) => 'b

@deprecated("Use `every` instead")
let everyU: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool
/** 
`every(m, p)` checks if all the bindings of the map satisfy the predicate
`p`. Order unspecified
*/
let every: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool

@deprecated("Use `some` instead")
let someU: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool
/**
`some(m, p)` checks if at least one binding of the map satisfy the
predicate `p`. Order unspecified
*/
let some: (t<'k, 'a, 'id>, ('k, 'a) => bool) => bool

let size: t<'k, 'a, 'id> => int

/** In increasing order. */
let toList: t<'k, 'a, 'id> => list<('k, 'a)>

let toArray: t<'k, 'a, 'id> => array<('k, 'a)>

let fromArray: (array<('k, 'a)>, ~cmp: cmp<'k, 'id>) => t<'k, 'a, 'id>

let keysToArray: t<'k, 'a, 'id> => array<'k>

let valuesToArray: t<'k, 'a, 'id> => array<'a>

let minKey: t<'k, _, _> => option<'k>

let minKeyUndefined: t<'k, _, _> => Js.undefined<'k>

let maxKey: t<'k, _, _> => option<'k>

let maxKeyUndefined: t<'k, _, _> => Js.undefined<'k>

let minimum: t<'k, 'a, _> => option<('k, 'a)>

let minUndefined: t<'k, 'a, _> => Js.undefined<('k, 'a)>

let maximum: t<'k, 'a, _> => option<('k, 'a)>

let maxUndefined: t<'k, 'a, _> => Js.undefined<('k, 'a)>

let get: (t<'k, 'a, 'id>, 'k, ~cmp: cmp<'k, 'id>) => option<'a>

let getUndefined: (t<'k, 'a, 'id>, 'k, ~cmp: cmp<'k, 'id>) => Js.undefined<'a>

let getWithDefault: (t<'k, 'a, 'id>, 'k, 'a, ~cmp: cmp<'k, 'id>) => 'a

let getExn: (t<'k, 'a, 'id>, 'k, ~cmp: cmp<'k, 'id>) => 'a

let getOrThrow: (t<'k, 'a, 'id>, 'k, ~cmp: cmp<'k, 'id>) => 'a

let checkInvariantInternal: t<_> => unit

/**
`remove(m, x)` returns a map containing the same bindings as `m`, except
for `x` which is unbound in the returned map.
*/
let remove: (t<'a, 'b, 'id>, 'a, ~cmp: cmp<'a, 'id>) => t<'a, 'b, 'id>

let removeMany: (t<'a, 'b, 'id>, array<'a>, ~cmp: cmp<'a, 'id>) => t<'a, 'b, 'id>

/**
`set(m, x, y)` returns a map containing the same bindings as `m`, plus a
binding of `x` to `y`. If `x` was already bound in `m`, its previous
binding disappears.
*/
let set: (t<'a, 'b, 'id>, 'a, 'b, ~cmp: cmp<'a, 'id>) => t<'a, 'b, 'id>

@deprecated("Use `update` instead")
let updateU: (t<'a, 'b, 'id>, 'a, option<'b> => option<'b>, ~cmp: cmp<'a, 'id>) => t<'a, 'b, 'id>
let update: (t<'a, 'b, 'id>, 'a, option<'b> => option<'b>, ~cmp: cmp<'a, 'id>) => t<'a, 'b, 'id>

@deprecated("Use `merge` instead")
let mergeU: (
  t<'a, 'b, 'id>,
  t<'a, 'c, 'id>,
  ('a, option<'b>, option<'c>) => option<'d>,
  ~cmp: cmp<'a, 'id>,
) => t<'a, 'd, 'id>
/** 
`merge(m1, m2, f)` computes a map whose keys is a subset of keys of `m1`
and of `m2`. The presence of each such binding, and the corresponding
value, is determined with the function `f`.
*/
let merge: (
  t<'a, 'b, 'id>,
  t<'a, 'c, 'id>,
  ('a, option<'b>, option<'c>) => option<'d>,
  ~cmp: cmp<'a, 'id>,
) => t<'a, 'd, 'id>

let mergeMany: (t<'a, 'b, 'id>, array<('a, 'b)>, ~cmp: cmp<'a, 'id>) => t<'a, 'b, 'id>

@deprecated("Use `keep` instead")
let keepU: (t<'k, 'a, 'id>, ('k, 'a) => bool) => t<'k, 'a, 'id>
/**
`keep(m, p)` returns the map with all the bindings in `m` that satisfy
predicate `p`.
*/
let keep: (t<'k, 'a, 'id>, ('k, 'a) => bool) => t<'k, 'a, 'id>

@deprecated("Use `partition` instead")
let partitionU: (t<'k, 'a, 'id>, ('k, 'a) => bool) => (t<'k, 'a, 'id>, t<'k, 'a, 'id>)
/** 
`partition(m, p)` returns a pair of maps `(m1, m2)`, where `m1` contains
all the bindings of `s` that satisfy the predicate `p`, and `m2` is the map
with all the bindings of `s` that do not satisfy `p`.
*/
let partition: (t<'k, 'a, 'id>, ('k, 'a) => bool) => (t<'k, 'a, 'id>, t<'k, 'a, 'id>)

/** 
`split(x, m)` returns a triple `(l, data, r)`, where `l` is the map with
all the bindings of `m` whose key is strictly less than `x`; `r` is the map
with all the bindings of `m` whose key is strictly greater than `x`; `data`
is `None` if `m` contains no binding for `x`, or `Some(v)` if `m` binds `v`
to `x`.
*/
let split: (
  t<'a, 'b, 'id>,
  'a,
  ~cmp: cmp<'a, 'id>,
) => ((t<'a, 'b, 'id>, t<'a, 'b, 'id>), option<'b>)

@deprecated("Use `map` instead")
let mapU: (t<'k, 'a, 'id>, 'a => 'b) => t<'k, 'b, 'id>
/** 
`map(m, f)` returns a map with same domain as `m`, where the associated
value `a` of all bindings of `m` has been replaced by the result of the
application of `f` to `a`. The bindings are passed to `f` in increasing
order with respect to the ordering over the type of the keys.
*/
let map: (t<'k, 'a, 'id>, 'a => 'b) => t<'k, 'b, 'id>

@deprecated("Use `mapWithKey` instead")
let mapWithKeyU: (t<'k, 'a, 'id>, ('k, 'a) => 'b) => t<'k, 'b, 'id>
let mapWithKey: (t<'k, 'a, 'id>, ('k, 'a) => 'b) => t<'k, 'b, 'id>
