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

external toOpt: opt<'a> => option<'a> = "%identity"
external return: 'a => opt<'a> = "%identity"

let emptyOpt: Js.undefined<'a>
let make: (~hash: 'hash, ~eq: 'eq, ~hintSize: int) => container<'hash, 'eq, _>

let clear: container<_> => unit

let isEmpty: container<_> => bool
