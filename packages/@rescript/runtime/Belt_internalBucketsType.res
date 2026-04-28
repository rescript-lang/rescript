/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */
type opt<'a> = Js.undefined<'a>

type container<'hash, 'eq, 'c> = {
  mutable size: int /* number of entries */,
  mutable buckets: array<opt<'c>> /* the buckets */,
  hash: 'hash,
  eq: 'eq,
}

module A = Belt_Array
external toOpt: opt<'a> => option<'a> = "%identity"
external return: 'a => opt<'a> = "%identity"

let emptyOpt = Js.undefined
let rec power_2_above = (x, n) =>
  if x >= n {
    x
  } else if x * 2 < x {
    x /* overflow */
  } else {
    power_2_above(x * 2, n)
  }

let make = (~hash, ~eq, ~hintSize) => {
  let s = power_2_above(16, hintSize)
  {
    size: 0,
    buckets: A.makeUninitialized(s),
    hash,
    eq,
  }
}

let clear = h => {
  h.size = 0
  let h_buckets = h.buckets
  let len = A.length(h_buckets)
  for i in 0 to len - 1 {
    A.setUnsafe(h_buckets, i, emptyOpt)
  }
}

let isEmpty = h => h.size == 0
