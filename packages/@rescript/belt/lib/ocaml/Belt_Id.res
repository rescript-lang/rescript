/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

type hash<'a, 'id> = 'a => int
type eq<'a, 'id> = ('a, 'a) => bool
type cmp<'a, 'id> = ('a, 'a) => int

external getHashInternal: hash<'a, 'id> => 'a => int = "%identity"
external getEqInternal: eq<'a, 'id> => ('a, 'a) => bool = "%identity"
external getCmpInternal: cmp<'a, 'id> => ('a, 'a) => int = "%identity"

module type Comparable = {
  type identity
  type t
  let cmp: cmp<t, identity>
}

type comparable<'key, 'id> = module(Comparable with type t = 'key and type identity = 'id)

module MakeComparable = (
  M: {
    type t
    let cmp: (t, t) => int
  },
) => {
  type identity
  include M
}

let comparable = (type key, ~cmp): module(Comparable with type t = key) =>
  module(
    MakeComparable({
      type t = key
      let cmp = cmp
    })
  )

module type Hashable = {
  type identity
  type t
  let hash: hash<t, identity>
  let eq: eq<t, identity>
}

type hashable<'key, 'id> = module(Hashable with type t = 'key and type identity = 'id)

module MakeHashable = (
  M: {
    type t
    let hash: t => int
    let eq: (t, t) => bool
  },
) => {
  type identity
  include M
}

let hashable = (type key, ~hash, ~eq): module(Hashable with type t = key) =>
  module(
    MakeHashable({
      type t = key
      let hash = hash
      let eq = eq
    })
  )

module MakeComparableU = MakeComparable
module MakeHashableU = MakeHashable

let comparableU = comparable
let hashableU = hashable
