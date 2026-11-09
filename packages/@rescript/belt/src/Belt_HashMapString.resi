type key = string

type t<'b>

let make: (~hintSize: int) => t<'b>

let clear: t<'b> => unit

let isEmpty: t<_> => bool

/**
`setDone(tbl, k, v)` if `k` does not exist, add the binding `k,v`, otherwise,
update the old value with the new `v`
*/
let set: (t<'a>, key, 'a) => unit

let copy: t<'a> => t<'a>
let get: (t<'a>, key) => option<'a>

let has: (t<'b>, key) => bool

/**
`getWithDefault(tbl, k, default)` returns the value bound to `k`, or `default` if `k` is not bound.

## Examples

```rescript
let hMap = Belt.HashMap.String.fromArray([("a", "value1")])

Belt.HashMap.String.getWithDefault(hMap, "a", "default") == "value1"
Belt.HashMap.String.getWithDefault(hMap, "b", "default") == "default"
```
*/
let getWithDefault: (t<'a>, key, 'a) => 'a

/**
`getOrThrow(tbl, k)` returns the value bound to `k`. Throws `Not_found` if `k` is not bound.

## Examples

```rescript
let hMap = Belt.HashMap.String.fromArray([("a", "value1")])

Belt.HashMap.String.getOrThrow(hMap, "a") == "value1"

switch Belt.HashMap.String.getOrThrow(hMap, "b") {
| exception Not_found => assert(true)
| _ => assert(false)
}
```
*/
let getOrThrow: (t<'a>, key) => 'a

let remove: (t<'a>, key) => unit

@deprecated("Use `forEach` instead")
let forEachU: (t<'b>, (key, 'b) => unit) => unit

let forEach: (t<'b>, (key, 'b) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'b>, 'c, ('c, key, 'b) => 'c) => 'c

let reduce: (t<'b>, 'c, ('c, key, 'b) => 'c) => 'c

@deprecated("Use `keepMapInPlace` instead")
let keepMapInPlaceU: (t<'a>, (key, 'a) => option<'a>) => unit

let keepMapInPlace: (t<'a>, (key, 'a) => option<'a>) => unit

let size: t<_> => int

let toArray: t<'a> => array<(key, 'a)>
let keysToArray: t<'a> => array<key>
let valuesToArray: t<'a> => array<'a>
let fromArray: array<(key, 'a)> => t<'a>
let mergeMany: (t<'a>, array<(key, 'a)>) => unit
let getBucketHistogram: t<_> => array<int>
let logStats: t<_> => unit
