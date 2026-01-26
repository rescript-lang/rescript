/***
Bindings to JavaScript's `WeakMap`.

Weak maps keep key/value pairs where keys must be objects and the references do not prevent garbage collection.
*/

/** Mutable weak map storing values of type `'v` with object keys `'k`. */
@notUndefined
type t<'k, 'v>

/**
Creates an empty weak map.

See [`WeakMap`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap) on MDN.

## Examples

```rescript
let cache = Stdlib_WeakMap.make()
Stdlib_WeakMap.get(cache, Stdlib_Object.make()) == None
```
*/
@new external make: unit => t<'k, 'v> = "WeakMap"

/**
`get(map, key)` returns `Some(value)` when `key` exists, otherwise `None`.

See [`WeakMap.prototype.get`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap/get) on MDN.

## Examples

```rescript
let cache = Stdlib_WeakMap.make()
let key = Stdlib_Object.make()
Stdlib_WeakMap.get(cache, key) == None
let _ = Stdlib_WeakMap.set(cache, key, "user")
Stdlib_WeakMap.get(cache, key) == Some("user")
```
*/
@send external get: (t<'k, 'v>, 'k) => option<'v> = "get"

/**
`has(map, key)` checks whether `key` exists in the weak map.

See [`WeakMap.prototype.has`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap/has) on MDN.

## Examples

```rescript
let cache = Stdlib_WeakMap.make()
let key = Stdlib_Object.make()
Stdlib_WeakMap.has(cache, key) == false
let _ = Stdlib_WeakMap.set(cache, key, ())
Stdlib_WeakMap.has(cache, key) == true
```
*/
@send external has: (t<'k, 'v>, 'k) => bool = "has"

/**
`set(map, key, value)` stores `value` for `key` and returns the map for chaining.

See [`WeakMap.prototype.set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap/set) on MDN.

## Examples

```rescript
let cache = Stdlib_WeakMap.make()
let key = Stdlib_Object.make()
let _ = Stdlib_WeakMap.set(cache, key, 42)
Stdlib_WeakMap.get(cache, key) == Some(42)
```
*/
@send external set: (t<'k, 'v>, 'k, 'v) => t<'k, 'v> = "set"

/**
`delete(map, key)` removes `key` and returns `true` if an entry existed.

See [`WeakMap.prototype.delete`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap/delete) on MDN.

## Examples

```rescript
let cache = Stdlib_WeakMap.make()
let key = Stdlib_Object.make()
Stdlib_WeakMap.delete(cache, key) == false
let _ = Stdlib_WeakMap.set(cache, key, 1)
Stdlib_WeakMap.delete(cache, key) == true
```
*/
@send external delete: (t<'k, 'v>, 'k) => bool = "delete"

/**
  `ignore(weakMap)` ignores the provided weakMap and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'k, 'v> => unit = "%ignore"
