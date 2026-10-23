/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module Int = Belt_SetInt
module String = Belt_SetString
module Dict = Belt_SetDict

type id<'value, 'id> = Belt_Id.comparable<'value, 'id>
type cmp<'value, 'id> = Belt_Id.cmp<'value, 'id>

type t<'value, 'id> = {
  cmp: cmp<'value, 'id>,
  data: Dict.t<'value, 'id>,
}

let fromArray = (type value identity, data, ~id: id<value, identity>) => {
  module M = unpack(id)
  let cmp = M.cmp
  {cmp, data: Dict.fromArray(~cmp, data)}
}

let remove = (m, e) => {
  let {cmp, data} = m
  let newData = Dict.remove(~cmp, data, e)
  if newData === data {
    m
  } else {
    {cmp, data: newData}
  }
}

let add = (m, e) => {
  let {cmp, data} = m
  let newData = Dict.add(~cmp, data, e)
  if newData === data {
    m
  } else {
    {cmp, data: newData}
  }
}

let mergeMany = ({cmp} as m, e) => {cmp, data: Dict.mergeMany(~cmp, m.data, e)}

let removeMany = ({cmp} as m, e) => {cmp, data: Dict.removeMany(~cmp, m.data, e)}

let union = ({cmp} as m, n) => {data: Dict.union(~cmp, m.data, n.data), cmp}

let intersect = (m, n) => {
  let cmp = m.cmp
  {data: Dict.intersect(~cmp, m.data, n.data), cmp}
}

let diff = (m, n) => {
  let cmp = m.cmp
  {cmp, data: Dict.diff(~cmp, m.data, n.data)}
}

let subset = (m, n) => {
  let cmp = m.cmp
  Dict.subset(~cmp, m.data, n.data)
}

let split = (m, e) => {
  let cmp = m.cmp
  let ((l, r), b) = Dict.split(~cmp, m.data, e)
  (({cmp, data: l}, {cmp, data: r}), b)
}

let make = (type value identity, ~id: id<value, identity>) => {
  module M = unpack(id)
  {cmp: M.cmp, data: Dict.empty}
}

let isEmpty = m => Dict.isEmpty(m.data)

let cmp = (m, n) => {
  let cmp = m.cmp
  Dict.cmp(~cmp, m.data, n.data)
}

let eq = (m, n) => Dict.eq(~cmp=m.cmp, m.data, n.data)

let forEach = (m, f) => Dict.forEach(m.data, f)

let reduce = (m, acc, f) => Dict.reduce(m.data, acc, f)

let every = (m, f) => Dict.every(m.data, f)

let some = (m, f) => Dict.some(m.data, f)

let keep = (m, f) => {cmp: m.cmp, data: Dict.keep(m.data, f)}

let partition = (m, f) => {
  let (l, r) = Dict.partition(m.data, f)
  let cmp = m.cmp
  ({data: l, cmp}, {data: r, cmp})
}

let size = m => Dict.size(m.data)
let toList = m => Dict.toList(m.data)
let toArray = m => Dict.toArray(m.data)

let minimum = m => Dict.minimum(m.data)
let maximum = m => Dict.maximum(m.data)

let get = (m, e) => Dict.get(~cmp=m.cmp, m.data, e)

let getOrThrow = (m, e) => Dict.getOrThrow(~cmp=m.cmp, m.data, e)

let getExn = getOrThrow

let has = (m, e) => Dict.has(~cmp=m.cmp, m.data, e)

let fromSortedArrayUnsafe = (type value identity, xs, ~id: id<value, identity>) => {
  module M = unpack(id)
  {cmp: M.cmp, data: Dict.fromSortedArrayUnsafe(xs)}
}

let getData = m => m.data

let getId = (type value identity, m: t<value, identity>): id<value, identity> => {
  module T = {
    type identity = identity
    type t = value
    let cmp = m.cmp
  }
  module(T)
}

let packIdData = (type value identity, ~id: id<value, identity>, ~data) => {
  module M = unpack(id)
  {cmp: M.cmp, data}
}

let checkInvariantInternal = d => Dict.checkInvariantInternal(d.data)

let everyU = every
let forEachU = forEach
let keepU = keep
let partitionU = partition
let reduceU = reduce
let someU = some
