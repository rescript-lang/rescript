/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module C = Belt_internalBucketsType

type rec bucket<'a, 'b> = {
  mutable key: 'a,
  mutable value: 'b,
  mutable next: C.opt<bucket<'a, 'b>>,
}
and t<'hash, 'eq, 'a, 'b> = C.container<'hash, 'eq, bucket<'a, 'b>>

let copy: t<'hash, 'eq, 'a, 'b> => t<'hash, 'eq, 'a, 'b>

let forEach: (t<_, _, 'a, 'b>, ('a, 'b) => 'c) => unit

let reduce: (t<_, _, 'a, 'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

let logStats: t<_> => unit

let keepMapInPlace: (t<_, _, 'a, 'b>, ('a, 'b) => option<'b>) => unit

let fillArray: (int, array<('a, 'b)>, bucket<'a, 'b>) => int

let keysToArray: t<_, _, 'a, _> => array<'a>

let valuesToArray: t<_, _, _, 'b> => array<'b>

let toArray: t<_, _, 'a, 'b> => array<('a, 'b)>

let getBucketHistogram: t<_> => array<int>
