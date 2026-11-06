/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module C = Belt_internalBucketsType

type rec bucket<'a> = {
  mutable key: 'a,
  mutable next: C.opt<bucket<'a>>,
}
and t<'hash, 'eq, 'a> = C.container<'hash, 'eq, bucket<'a>>

let copy: t<'hash, 'eq, 'a> => t<'hash, 'eq, 'a>

let forEach: (t<'hash, 'eq, 'a>, 'a => unit) => unit
let fillArray: (int, array<'a>, bucket<'a>) => int

let toArray: t<_, _, 'a> => array<'a>

let reduce: (t<_, _, 'a>, 'b, ('b, 'a) => 'b) => 'b

let logStats: t<_> => unit

let getBucketHistogram: t<_> => array<int>
