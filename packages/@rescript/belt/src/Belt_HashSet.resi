/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
A **mutable** Hash set which allows customized `hash` behavior.

All data are parameterized by not its only type but also a unique identity in
the time of initialization, so that two _HashSets of ints_ initialized with
different _hash_ functions will have different type.

## Examples

```rescript
module I0 = unpack(Belt.Id.hashable(~hash=(a: int) => land(a, 65535), ~eq=(a, b) => a == b))

let s0 = Belt.HashSet.make(~id=module(I0), ~hintSize=40)

module I1 = unpack(Belt.Id.hashable(~hash=(a: int) => land(a, 255), ~eq=(a, b) => a == b))

let s1 = Belt.HashSet.make(~id=module(I1), ~hintSize=40)

Belt.HashSet.add(s1, 0)
Belt.HashSet.add(s1, 1)
```

The invariant must be held: for two elements who are equal, their hashed
value should be the same.

Here the compiler would infer `s0` and `s1` having different type so that it
would not mix.

Signatures:

```
let s0: Belt.HashSet.t<int, I0.identity>
let s1: Belt.HashSet.t<int, I1.identity>
```

We can add elements to the collection (see last two lines in the example
above). Since this is an mutable data structure, `s1` will contain two pairs.
*/

/** Specalized when key type is `int`, more efficient than the generic type */
module Int = Belt_HashSetInt

/** Specalized when key type is `string`, more efficient than the generic type */
module String = Belt_HashSetString

/* TODO: add a poly module
   module Poly = Belt_HashSetPoly
   challenge:
   - generic equal handles JS data structure
   - eq/hash consistent
*/

type t<'a, 'id>

/** The type of hash tables from type `'a` to type `'b`. */
type id<'a, 'id> = Belt_Id.hashable<'a, 'id>

let make: (~hintSize: int, ~id: id<'a, 'id>) => t<'a, 'id>
let clear: t<'a, 'id> => unit
let isEmpty: t<_> => bool

let add: (t<'a, 'id>, 'a) => unit

let copy: t<'a, 'id> => t<'a, 'id>

let has: (t<'a, 'id>, 'a) => bool

let remove: (t<'a, 'id>, 'a) => unit

@deprecated("Use `forEach` instead")
let forEachU: (t<'a, 'id>, 'a => unit) => unit
/** Order unspecified. */
let forEach: (t<'a, 'id>, 'a => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'a, 'id>, 'c, ('c, 'a) => 'c) => 'c
/** Order unspecified. */
let reduce: (t<'a, 'id>, 'c, ('c, 'a) => 'c) => 'c

let size: t<'a, 'id> => int

let logStats: t<_> => unit

let toArray: t<'a, 'id> => array<'a>

let fromArray: (array<'a>, ~id: id<'a, 'id>) => t<'a, 'id>

let mergeMany: (t<'a, 'id>, array<'a>) => unit

let getBucketHistogram: t<_> => array<int>
