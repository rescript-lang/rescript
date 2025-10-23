/***
Bindings to JavaScript's `WeakSet`.

Weak sets store object references without preventing those objects from being garbage collected.
*/

/** Mutable weak set storing object references of type `'a`. */
@notUndefined
type t<'a>

/**
Creates an empty weak set.

See [`WeakSet`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet) on MDN.

## Examples

```rescript
let visited: WeakSet.t<{..}> = WeakSet.make()
```
*/
@new external make: unit => t<'a> = "WeakSet"

/**
`add(set, value)` inserts `value` into the weak set and returns the set for chaining.

## Examples

```rescript
let set = WeakSet.make()
let node = Stdlib_Object.make()
WeakSet.add(set, node)
WeakSet.has(set, node) == true
```
*/
@send external add: (t<'a>, 'a) => t<'a> = "add"

/**
`delete(set, value)` removes `value` and returns `true` if an entry existed.
*/
@send external delete: (t<'a>, 'a) => bool = "delete"

/**
`has(set, value)` checks whether `value` exists in the weak set.
*/
@send external has: (t<'a>, 'a) => bool = "has"

/**
  `ignore(weakSet)` ignores the provided weakSet and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"
