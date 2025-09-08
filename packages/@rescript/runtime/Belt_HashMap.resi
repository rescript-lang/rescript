/* Copyright (C) 2018 Authors of ReScript
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
A **mutable** Hash map which allows customized [`hash`]() behavior.

All data are parameterized by not its only type but also a unique identity in
the time of initialization, so that two _HashMaps of ints_ initialized with different
_hash_ functions will have different type.

## Examples

```rescript
type t = int
module I0 = unpack(Belt.Id.hashable(~hash=(_: t) => 0xff_ff, ~eq=(a, b) => a == b))
let s0: Belt.HashMap.t<I0.t, int, I0.identity> = Belt.HashMap.make(~hintSize=40, ~id=module(I0))

module I1 = unpack(Belt.Id.hashable(~hash=(_: t) => 0xff, ~eq=(a, b) => a == b))
let s1: Belt.HashMap.t<I1.t, string, I1.identity> = Belt.HashMap.make(~hintSize=40, ~id=module(I1))
```

The invariant must be held: for two elements who are _equal_,
their hashed value should be the same

Here the compiler would infer `s0` and `s1` having different type so that
it would not mix.

## Examples

```
let s0: t<int, I0.identity>
let s1: t<int, I1.identity>
```

We can add elements to the collection:

## Examples

```rescript
let () = {
  Belt.HashMap.set(s0, 0, 3)
  Belt.HashMap.set(s1, 1, "3")
}
```

Since this is an mutable data strucure, `s1` will contain two pairs.
*/

/** Specalized when key type is `int`, more efficient than the generic type */
module Int = Belt_HashMapInt

/** Specalized when key type is `string`, more efficient than the generic type */
module String = Belt_HashMapString

/** The type of hash tables from type `'key` to type `'value`. */
type t<'key, 'value, 'id>

/** The identity needed for making an empty hash map. */
type id<'a, 'id> = Belt_Id.hashable<'a, 'id>

/**
`make(~hintSize=10, ~id)` creates a new map by taking in the comparator and `hintSize`.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

Belt.HashMap.set(hMap, 0, "a")
```
*/
let make: (~hintSize: int, ~id: id<'key, 'id>) => t<'key, 'value, 'id>

/* TODO: allow randomization for security */

/** 
Clears a hash table.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let hMap = Belt.HashMap.fromArray([(1, "1")], ~id=module(IntHash))
Belt.HashMap.clear(hMap)
Belt.HashMap.isEmpty(hMap) == true
```
*/
let clear: t<'key, 'value, 'id> => unit

/**
`isEmpty(m)` checks whether a hash map is empty.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

Belt.HashMap.isEmpty(Belt.HashMap.fromArray([(1, "1")], ~id=module(IntHash))) == false
```
*/
let isEmpty: t<_> => bool

/**
`set(hMap, k, v)` if `k` does not exist, add the binding `k,v`, otherwise, update the old value with the new `v`.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntHash))

Belt.HashMap.set(s0, 2, "3")

Belt.HashMap.valuesToArray(s0) == ["1", "3", "3"]
```
*/
let set: (t<'key, 'value, 'id>, 'key, 'value) => unit

/**
Creates copy of a hash map.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntHash))
let s1 = Belt.HashMap.copy(s0)

Belt.HashMap.set(s0, 2, "3")

(Belt.HashMap.get(s0, 2) != Belt.HashMap.get(s1, 2)) == true
```
*/
let copy: t<'key, 'value, 'id> => t<'key, 'value, 'id>

/**
Returns value bound under specific key. If values not exist returns `None`.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")

Belt.HashMap.get(s0, 1) == Some("value1")
Belt.HashMap.get(s0, 2) == None
```
*/
let get: (t<'key, 'value, 'id>, 'key) => option<'value>

/** 
Checks if `x` is bound in `tbl`.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")

Belt.HashMap.has(s0, 1) == true
Belt.HashMap.has(s0, 2) == false
```
*/
let has: (t<'key, 'value, 'id>, 'key) => bool

/**
If bound exists, removes it from the hash map.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.remove(s0, 1)
Belt.HashMap.has(s0, 1) == false
```
*/
let remove: (t<'key, 'value, 'id>, 'key) => unit

/** Same as [forEach](#forEach) but takes uncurried function. */
@deprecated("Use `forEach` instead")
let forEachU: (t<'key, 'value, 'id>, ('key, 'value) => unit) => unit

/**
`forEach(tbl, f)` applies `f` to all bindings in table `tbl`. `f` receives the key as first argument, and the associated value as second argument. Each binding is presented exactly once to `f`.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.forEach(s0, (key, value) => Js.log2(key, value))
// prints (1, "value1")
```
*/
let forEach: (t<'key, 'value, 'id>, ('key, 'value) => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'key, 'value, 'id>, 'c, ('c, 'key, 'value) => 'c) => 'c
/**
`reduce(tbl, init, f)` computes `(f(kN, dN) ... (f(k1, d1, init))...)`, where `k1 ... kN` are the keys of all bindings in `tbl`, and `d1 ... dN` are the associated values. Each binding is presented exactly once to `f`.

The order in which the bindings are passed to `f` is unspecified. However, if the table contains several bindings for the same key, they are passed to `f` in reverse order of introduction, that is, the most recent binding is passed first.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.set(s0, 2, "value2")

s0->Belt.HashMap.reduce("", (acc, _, value) => acc ++ (", " ++ value)) == ", value1, value2"
```

## More Examples

```rescript
Console.log("lol")
```
*/
let reduce: (t<'key, 'value, 'id>, 'c, ('c, 'key, 'value) => 'c) => 'c

/** Same as [keepMapInPlace](#keepMapInPlace) but takes uncurried function.  */
@deprecated("Use `keepMapInPlace` instead")
let keepMapInPlaceU: (t<'key, 'value, 'id>, ('key, 'value) => option<'value>) => unit

/**
Filters out values for which function `f` returned `None`.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.set(s0, 2, "value2")

Belt.HashMap.keepMapInPlace(s0, (key, value) => key == 1 ? None : Some(value))
```
*/
let keepMapInPlace: (t<'key, 'value, 'id>, ('key, 'value) => option<'value>) => unit

/** 
`size(tbl)` returns the number of bindings in `tbl`. It takes constant time.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.set(s0, 2, "value2")

Belt.HashMap.size(s0) == 2
```
*/
let size: t<_> => int

/**
Returns array of key value pairs.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.set(s0, 2, "value2")

Belt.HashMap.toArray(s0) == [(1, "value1"), (2, "value2")]
```
*/
let toArray: t<'key, 'value, 'id> => array<('key, 'value)>

/**
Returns array of keys.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.set(s0, 2, "value2")

Belt.HashMap.keysToArray(s0) == [1, 2]
```
 */
let keysToArray: t<'key, _, _> => array<'key>

/**
Returns array of values.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(s0, 1, "value1")
Belt.HashMap.set(s0, 2, "value2")

Belt.HashMap.valuesToArray(s0) == ["value1", "value2"]
```
*/
let valuesToArray: t<_, 'value, _> => array<'value>

/**
Creates new hash map from array of pairs.

Returns array of values.

## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let s0 = Belt.HashMap.fromArray([(1, "value1"), (2, "value2")], ~id=module(IntHash))
Belt.HashMap.toArray(s0) == [(1, "value1"), (2, "value2")]
```
*/
let fromArray: (array<('key, 'value)>, ~id: id<'key, 'id>) => t<'key, 'value, 'id>

/**
## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.mergeMany(hMap, [(1, "1"), (2, "2")])
```
*/
let mergeMany: (t<'key, 'value, 'id>, array<('key, 'value)>) => unit

/**
## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})
let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(hMap, 1, "1")

Belt.HashMap.getBucketHistogram(hMap)
```
*/
let getBucketHistogram: t<_> => array<int>

/**
## Examples

```rescript
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})
let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
Belt.HashMap.set(hMap, 1, "1")

Belt.HashMap.logStats(hMap)
```
*/
let logStats: t<_> => unit
