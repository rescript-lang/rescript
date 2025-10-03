/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* Adapted by authors of ReScript without using functors */
/* ********************************************************************* */

/***
The top level provides generic immutable map operations.

It also has three specialized inner modules `Belt.Map.Int`, `Belt.Map.String`
and `Belt.Map.Dict`.
*/

/**
## Examples

```rescript
type t<'key, 'value, 'identity>
type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
```
*/
module Int = Belt_MapInt

module String = Belt_MapString

module Dict = Belt_MapDict

/**
`'key` is the field type

`'value` is the element type

`'identity` the identity of the collection
*/
type t<'key, 'value, 'identity>

/** The identity needed for making an empty map. */
type id<'key, 'id> = Belt_Id.comparable<'key, 'id>

/**
`make(~id)` creates a new map by taking in the comparator.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let m = Belt.Map.make(~id=module(IntCmp))

Belt.Map.set(m, 0, "a")
```
*/
let make: (~id: id<'k, 'id>) => t<'k, 'v, 'id>

/**

`isEmpty(m)` checks whether a map m is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.isEmpty(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp))) == false
```
*/
let isEmpty: t<_> => bool

/**
`has(m, k)` checks whether `m` has the key `k`.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.has(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp)), 1) == true
```
*/
let has: (t<'k, 'v, 'id>, 'k) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'k, 'v, 'id>, t<'k, 'v, 'id>, ('v, 'v) => int) => int
/** 
`cmp(m0, m1, vcmp);`

Total ordering of map given total ordering of value function.

It will compare size first and each element following the order one by one.
*/
let cmp: (t<'k, 'v, 'id>, t<'k, 'v, 'id>, ('v, 'v) => int) => int

@deprecated("Use `eq` instead")
let eqU: (t<'k, 'v, 'id>, t<'k, 'v, 'id>, ('v, 'v) => bool) => bool
/**
eq(m1, m2, veq)` tests whether the maps `m1` and `m2` are equal, that is,
contain equal keys and associate them with equal data. `veq` is the
equality predicate used to compare the data associated with the keys. 
*/
let eq: (t<'k, 'v, 'id>, t<'k, 'v, 'id>, ('v, 'v) => bool) => bool

@deprecated("Use `findFirstBy` instead")
let findFirstByU: (t<'k, 'v, 'id>, ('k, 'v) => bool) => option<('k, 'v)>
/** `
findFirstBy(m, p)` uses function `f` to find the first key value pair to match predicate `p`.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

s0->Belt.Map.findFirstBy((k, _) => k == 4) == Some(4, "4")
```
*/
let findFirstBy: (t<'k, 'v, 'id>, ('k, 'v) => bool) => option<('k, 'v)>

@deprecated("Use `forEach` instead")
let forEachU: (t<'k, 'v, 'id>, ('k, 'v) => unit) => unit
/** 
 `forEach(m, f)` applies `f` to all bindings in map `m`. `f` receives the
`'k` as first argument, and the associated value as second argument. The
bindings are passed to `f` in increasing order with respect to the ordering
over the type of the keys.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "3")])

let acc = ref(list{})

Belt.Map.forEach(s0, (k, v) => acc := list{(k, v), ...acc.contents})

acc.contents == list{(4, "4"), (3, "3"), (2, "2"), (1, "1")}
```
*/
let forEach: (t<'k, 'v, 'id>, ('k, 'v) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'k, 'v, 'id>, 'acc, ('acc, 'k, 'v) => 'acc) => 'acc
/** 
 `reduce(m, a, f)` computes `(f(kN, dN) ... (f(k1, d1, a))...)`, where `k1
... kN` are the keys of all bindings in m (in increasing order), and `d1
... dN` are the associated data.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "3")])

Belt.Map.reduce(s0, list{}, (acc, k, v) => list{(k, v), ...acc}) ==
  list{(4, "4"), (3, "3"), (2, "2"), (1, "1")}
```
*/
let reduce: (t<'k, 'v, 'id>, 'acc, ('acc, 'k, 'v) => 'acc) => 'acc

@deprecated("Use `every` instead")
let everyU: (t<'k, 'v, 'id>, ('k, 'v) => bool) => bool
/**
`every(m, p)` checks if all the bindings of the map satisfy the predicate
`p`. Order unspecified
*/
let every: (t<'k, 'v, 'id>, ('k, 'v) => bool) => bool

@deprecated("Use `some` instead")
let someU: (t<'k, 'v, 'id>, ('k, 'v) => bool) => bool
/**
`some(m, p)` checks if at least one binding of the map satisfy the predicate 
`p`. Order unspecified */
let some: (t<'k, 'v, 'id>, ('k, 'v) => bool) => bool

/** 
 `size(s)`

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.size(Belt.Map.fromArray([(2, "2"), (2, "1"), (3, "3")], ~id=module(IntCmp))) == 2
```
*/
let size: t<'k, 'v, 'id> => int

/** 
 `toArray(s)`

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
    (1, "1"),
    (2, "2"),
    (3, "3"),
  ]
```
*/
let toArray: t<'k, 'v, 'id> => array<('k, 'v)>

/** In increasing order. See `Belt.Map.toArray`*/
let toList: t<'k, 'v, 'id> => list<('k, 'v)>

/** 
`fromArray(kvs, ~id);`

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
    (1, "1"),
    (2, "2"),
    (3, "3"),
  ]
```
*/
let fromArray: (array<('k, 'v)>, ~id: id<'k, 'id>) => t<'k, 'v, 'id>

/** 
`keysToArray(s);`

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.keysToArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
    1,
    2,
    3,
  ]
```
*/
let keysToArray: t<'k, 'v, 'id> => array<'k>

/** 
 `valuesToArray(s);`

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.valuesToArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
    "1",
    "2",
    "3",
  ]
```
*/
let valuesToArray: t<'k, 'v, 'id> => array<'v>

/** `minKey(s)` returns the minimum key, None if not exist. */
let minKey: t<'k, _, _> => option<'k>

/** See `Belt.Map.minKey` */
let minKeyUndefined: t<'k, _, _> => Js.undefined<'k>

/** `maxKey(s)` returns the maximum key, None if not exist. */
let maxKey: t<'k, _, _> => option<'k>

/** See `Belt.Map.maxKey` */
let maxKeyUndefined: t<'k, _, _> => Js.undefined<'k>

/** `minimum(s)` returns the minimum key value pair, None if not exist. */
let minimum: t<'k, 'v, _> => option<('k, 'v)>

/** See `Belt.Map.minimum` */
let minUndefined: t<'k, 'v, _> => Js.undefined<('k, 'v)>

/** `maximum(s)` returns the maximum key value pair, None if not exist. */
let maximum: t<'k, 'v, _> => option<('k, 'v)>

/** See `Belt.Map.maximum` */
let maxUndefined: t<'k, 'v, _> => Js.undefined<('k, 'v)>

/** 
 `get(s, k)`

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) == Some("2")

Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 4) == None
```
*/
let get: (t<'k, 'v, 'id>, 'k) => option<'v>

/** See `Belt.Map.get`. Returns `undefined` when not found*/
let getUndefined: (t<'k, 'v, 'id>, 'k) => Js.undefined<'v>

/** 
`getWithDefault(s, k, default)`

See `Belt.Map.get`

Returns default when `k` is not found.
*/
let getWithDefault: (t<'k, 'v, 'id>, 'k, 'v) => 'v

/** 
`getExn(s, k)`

See `Belt.Map.get`

throw when `k` not exist
*/
let getExn: (t<'k, 'v, 'id>, 'k) => 'v

/**
`getOrThrow(s, k)`

See `Belt.Map.get`

throw when `k` not exist
*/
let getOrThrow: (t<'k, 'v, 'id>, 'k) => 'v

/* ************************************************************************** */

/** 
`remove(m, x)` when `x` is not in `m`, `m` is returned reference unchanged.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

let s1 = Belt.Map.remove(s0, 1)

let s2 = Belt.Map.remove(s1, 1)

s1 == s2

Belt.Map.keysToArray(s1) == [2, 3]
```
*/
let remove: (t<'k, 'v, 'id>, 'k) => t<'k, 'v, 'id>

/** 
`removeMany(s, xs)`

Removing each of `xs` to `s`, note unlike `Belt.Map.remove`, the reference
of return value might be changed even if none in `xs` exists `s`.
*/
let removeMany: (t<'k, 'v, 'id>, array<'k>) => t<'k, 'v, 'id>

/** 
`set(m, x, y)` returns a map containing the same bindings as `m`, with a
new binding of `x` to `y`. If `x` was already bound in `m`, its previous
binding disappears.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

let s1 = Belt.Map.set(s0, 2, "3")

Belt.Map.valuesToArray(s1) == ["1", "3", "3"]
```
*/
let set: (t<'k, 'v, 'id>, 'k, 'v) => t<'k, 'v, 'id>

@deprecated("Use `update` instead")
let updateU: (t<'k, 'v, 'id>, 'k, option<'v> => option<'v>) => t<'k, 'v, 'id>
/** 
`update(m, x, f)` returns a map containing the same bindings as `m`, except
for the binding of `x`. Depending on the value of `y` where `y` is
`f(get(m, x))`, the binding of `x` is added, removed or updated. If `y` is
`None`, the binding is removed if it exists; otherwise, if `y` is `Some(z)`
then `x` is associated to `z` in the resulting map. 
*/
let update: (t<'k, 'v, 'id>, 'k, option<'v> => option<'v>) => t<'k, 'v, 'id>

/** 
 `mergeMany(s, xs)`

Adding each of `xs` to `s`, note unlike `add`, the reference of return
value might be changed even if all values in `xs` exist `s`.
*/
let mergeMany: (t<'k, 'v, 'id>, array<('k, 'v)>) => t<'k, 'v, 'id>

@deprecated("Use `merge` instead")
let mergeU: (
  t<'k, 'v, 'id>,
  t<'k, 'v2, 'id>,
  ('k, option<'v>, option<'v2>) => option<'v3>,
) => t<'k, 'v3, 'id>
/** 
`merge(m1, m2, f)` computes a map whose keys is a subset of keys of `m1`
and of `m2`. The presence of each such binding, and the corresponding
value, is determined with the function `f`. 
*/
let merge: (
  t<'k, 'v, 'id>,
  t<'k, 'v2, 'id>,
  ('k, option<'v>, option<'v2>) => option<'v3>,
) => t<'k, 'v3, 'id>

@deprecated("Use `keep` instead")
let keepU: (t<'k, 'v, 'id>, ('k, 'v) => bool) => t<'k, 'v, 'id>
/**
`keep(m, p)` returns the map with all the bindings in m that satisfy
predicate `p`.
*/
let keep: (t<'k, 'v, 'id>, ('k, 'v) => bool) => t<'k, 'v, 'id>

@deprecated("Use `partition` instead")
let partitionU: (t<'k, 'v, 'id>, ('k, 'v) => bool) => (t<'k, 'v, 'id>, t<'k, 'v, 'id>)
/**
`partition(m, p)` returns a pair of maps `(m1, m2)`, where `m1` contains
all the bindings of `s` that satisfy the predicate `p`, and `m2` is the map
with all the bindings of `s` that do not satisfy `p`.
*/
let partition: (t<'k, 'v, 'id>, ('k, 'v) => bool) => (t<'k, 'v, 'id>, t<'k, 'v, 'id>)

/** 
`split(x, m)` returns a tuple `(l, r)`, data, where `l` is the map with all
the bindings of `m` whose 'k is strictly less than `x`; `r` is the map with
all the bindings of m whose 'k is strictly greater than `x`; `data` is
`None` if `m` contains no binding for `x`, or `Some(v)` if `m` binds `v` to
`x`.
*/
let split: (t<'k, 'v, 'id>, 'k) => ((t<'k, 'v, 'id>, t<'k, 'v, 'id>), option<'v>)

@deprecated("Use `map` instead")
let mapU: (t<'k, 'v, 'id>, 'v => 'v2) => t<'k, 'v2, 'id>
/**
`map(m, f)` returns a map with same domain as `m`, where the associated value `a`
of all bindings of `m` has been replaced by the result of the application of `f`
to `a`. The bindings are passed to `f` in increasing order with respect to the
ordering over the type of the keys.
*/
let map: (t<'k, 'v, 'id>, 'v => 'v2) => t<'k, 'v2, 'id>

@deprecated("Use `mapWithKey` instead")
let mapWithKeyU: (t<'k, 'v, 'id>, ('k, 'v) => 'v2) => t<'k, 'v2, 'id>
/** 
`mapWithKey(m, f)`

The same as `Belt.Map.map` except that `f` is supplied with one more
argument: the key.
*/
let mapWithKey: (t<'k, 'v, 'id>, ('k, 'v) => 'v2) => t<'k, 'v2, 'id>

/** 
`getData(s0)`

Advanced usage only

Returns the raw data (detached from comparator), but its type is still
manifested, so that user can pass identity directly without boxing.
*/
let getData: t<'k, 'v, 'id> => Belt_MapDict.t<'k, 'v, 'id>

/**
Advanced usage only. Returns the identity of s0.
*/
let getId: t<'k, 'v, 'id> => id<'k, 'id>

/** 
`packIdData(~id, ~data)`

Advanced usage only

Returns the packed collection.
*/
let packIdData: (~id: id<'k, 'id>, ~data: Belt_MapDict.t<'k, 'v, 'id>) => t<'k, 'v, 'id>

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t<_> => unit
