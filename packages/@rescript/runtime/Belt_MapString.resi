type key = string

/** The type of maps from type `key` to type `'value`. */
type t<'value>

let empty: t<'v>

let isEmpty: t<'v> => bool

let has: (t<'v>, key) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'v>, t<'v>, ('v, 'v) => int) => int
let cmp: (t<'v>, t<'v>, ('v, 'v) => int) => int

@deprecated("Use `eq` instead")
let eqU: (t<'v>, t<'v>, ('v, 'v) => bool) => bool

/**
`eq(m1, m2)` tests whether the maps `m1` and `m2` are
equal, that is, contain equal keys and associate them with
equal data.
*/
let eq: (t<'v>, t<'v>, ('v, 'v) => bool) => bool

@deprecated("Use `findFirstBy` instead")
let findFirstByU: (t<'v>, (key, 'v) => bool) => option<(key, 'v)>

/**
`findFirstBy(m, p)` uses funcion `f` to find the first key value pair
to match predicate `p`.

## Examples

```rescript
let mapString = Belt.Map.String.fromArray([("1", "one"), ("2", "two"), ("3", "three")])

mapString->Belt.Map.String.findFirstBy((k, v) => k == "1" && v == "one") == Some("1", "one")
```
*/
let findFirstBy: (t<'v>, (key, 'v) => bool) => option<(key, 'v)>

@deprecated("Use `forEach` instead")
let forEachU: (t<'v>, (key, 'v) => unit) => unit

/**
`forEach(m, f)` applies `f` to all bindings in map `m`.
`f` receives the key as first argument, and the associated value
as second argument. The bindings are passed to `f` in increasing
order with respect to the ordering over the type of the keys.
*/
let forEach: (t<'v>, (key, 'v) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'v>, 'v2, ('v2, key, 'v) => 'v2) => 'v2

/**
`reduce(m, a, f)` computes `(f kN dN ... (f k1 d1 a)...)`,
where `k1 ... kN` are the keys of all bindings in `m`
(in increasing order), and `d1 ... dN` are the associated data.
*/
let reduce: (t<'v>, 'v2, ('v2, key, 'v) => 'v2) => 'v2

@deprecated("Use `every` instead")
let everyU: (t<'v>, (key, 'v) => bool) => bool

/** 
`every(m, p)` checks if all the bindings of the map satisfy the predicate `p`.
Order unspecified */
let every: (t<'v>, (key, 'v) => bool) => bool

@deprecated("Use `some` instead")
let someU: (t<'v>, (key, 'v) => bool) => bool

/** 
`some(m, p)` checks if at least one binding of the map satisfy the predicate
`p`. Order unspecified */
let some: (t<'v>, (key, 'v) => bool) => bool

let size: t<'v> => int

/** In increasing order. */
let toList: t<'v> => list<(key, 'v)>

let toArray: t<'v> => array<(key, 'v)>

let fromArray: array<(key, 'v)> => t<'v>

let keysToArray: t<'v> => array<key>

let valuesToArray: t<'v> => array<'v>

let minKey: t<_> => option<key>

let minKeyUndefined: t<_> => Js.undefined<key>

let maxKey: t<_> => option<key>

let maxKeyUndefined: t<_> => Js.undefined<key>

let minimum: t<'v> => option<(key, 'v)>

let minUndefined: t<'v> => Js.undefined<(key, 'v)>

let maximum: t<'v> => option<(key, 'v)>

let maxUndefined: t<'v> => Js.undefined<(key, 'v)>

let get: (t<'v>, key) => option<'v>

let getUndefined: (t<'v>, key) => Js.undefined<'v>

let getWithDefault: (t<'v>, key, 'v) => 'v

let getExn: (t<'v>, key) => 'v

let getOrThrow: (t<'v>, key) => 'v

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t<_> => unit

/** `remove m x` returns a map containing the same bindings as
    `m`, except for `x` which is unbound in the returned map. */
let remove: (t<'v>, key) => t<'v>

let removeMany: (t<'v>, array<key>) => t<'v>

/**
`set(m, x, y)` returns a map containing the same bindings as
`m`, plus a binding of `x` to `y`. If `x` was already bound
in `m`, its previous binding disappears.
*/
let set: (t<'v>, key, 'v) => t<'v>

@deprecated("Use `update` instead")
let updateU: (t<'v>, key, option<'v> => option<'v>) => t<'v>
let update: (t<'v>, key, option<'v> => option<'v>) => t<'v>

@deprecated("Use `merge` instead")
let mergeU: (t<'v>, t<'v2>, (key, option<'v>, option<'v2>) => option<'c>) => t<'c>

/**
`merge(m1, m2, f)` computes a map whose keys is a subset of keys of `m1`
and of `m2`. The presence of each such binding, and the corresponding
value, is determined with the function `f`.
*/
let merge: (t<'v>, t<'v2>, (key, option<'v>, option<'v2>) => option<'c>) => t<'c>

let mergeMany: (t<'v>, array<(key, 'v)>) => t<'v>

@deprecated("Use `keep` instead")
let keepU: (t<'v>, (key, 'v) => bool) => t<'v>

/** 
`keep(m, p)` returns the map with all the bindings in `m` that satisfy predicate
`p`.
*/
let keep: (t<'v>, (key, 'v) => bool) => t<'v>

@deprecated("Use `partition` instead")
let partitionU: (t<'v>, (key, 'v) => bool) => (t<'v>, t<'v>)

/**
`partition(m, p)` returns a pair of maps `(m1, m2)`, where `m1` contains all the
bindings of `s` that satisfy the predicate `p`, and `m2` is the map with all the
bindings of `s` that do not satisfy `p`.
*/
let partition: (t<'v>, (key, 'v) => bool) => (t<'v>, t<'v>)

/**
`split(x, m)` returns a triple `(l, data, r)`, where `l` is the map with all the
bindings of `m` whose key is strictly less than `x`; `r` is the map with all the
bindings of `m` whose key is strictly greater than `x`; `data` is `None` if `m`
contains no binding for `x`, or `Some(v)` if `m` binds `v` to `x`.
*/
let split: (key, t<'v>) => (t<'v>, option<'v>, t<'v>)

@deprecated("Use `map` instead")
let mapU: (t<'v>, 'v => 'v2) => t<'v2>

/**
`map(m, f)` returns a map with same domain as `m`, where the associated value `a`
of all bindings of `m` has been replaced by the result of the application of `f`
to `a`. The bindings are passed to `f` in increasing order with respect to the
ordering over the type of the keys.
*/
let map: (t<'v>, 'v => 'v2) => t<'v2>

@deprecated("Use `mapWithKey` instead")
let mapWithKeyU: (t<'v>, (key, 'v) => 'v2) => t<'v2>
let mapWithKey: (t<'v>, (key, 'v) => 'v2) => t<'v2>
