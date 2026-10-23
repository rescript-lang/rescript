/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
This module is [`Belt.HashSet`]() specialized with key type to be a primitive type.

It is more efficient in general, the  API is the same with [`Belt.HashSet`]() except its key type is fixed,
and identity is not needed(using the built-in one)

**See** [`Belt.HashSet`]()
*/

type key = int

type t

let make: (~hintSize: int) => t

let clear: t => unit

let isEmpty: t => bool

let add: (t, key) => unit

let copy: t => t

let has: (t, key) => bool

let remove: (t, key) => unit

@deprecated("Use `forEach` instead")
let forEachU: (t, key => unit) => unit
let forEach: (t, key => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t, 'c, ('c, key) => 'c) => 'c
let reduce: (t, 'c, ('c, key) => 'c) => 'c

let size: t => int

let logStats: t => unit

let toArray: t => array<key>

let fromArray: array<key> => t

let mergeMany: (t, array<key>) => unit

let getBucketHistogram: t => array<int>
