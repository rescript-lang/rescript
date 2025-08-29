/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* Adapted by authors of ReScript without using functors */
/* ********************************************************************* */

/** specalized when key type is `int`, more efficient
    than the generic type
*/
module Int = Belt_MapInt

/** specalized when key type is `string`, more efficient
    than the generic type */
module String = Belt_MapString

/** seprate function from data, a more verboe but slightly
    more efficient
*/
module Dict = Belt_MapDict

type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

type t<'k, 'v, 'id> = {
  cmp: cmp<'k, 'id>,
  data: Dict.t<'k, 'v, 'id>,
}

let fromArray = (type k idx, data, ~id: id<k, idx>) => {
  module M = unpack(id)
  let cmp = M.cmp
  {cmp, data: Dict.fromArray(~cmp, data)}
}

let remove = (m, x) => {
  let {cmp, data: odata} = m
  let newData = Dict.remove(odata, x, ~cmp)
  if newData === odata {
    m
  } else {
    {cmp, data: newData}
  }
}

let removeMany = (m, x) => {
  let {cmp, data: odata} = m
  let newData = Dict.removeMany(odata, x, ~cmp)
  {cmp, data: newData}
}

let set = (m, key, d) => {
  let cmp = m.cmp
  {cmp, data: Dict.set(~cmp, m.data, key, d)}
}

let mergeMany = (m, e) => {
  let cmp = m.cmp
  {cmp, data: Dict.mergeMany(~cmp, m.data, e)}
}

let update = (m, key, f) => {
  let cmp = m.cmp
  {cmp, data: Dict.update(~cmp, m.data, key, f)}
}
let split = (m, x) => {
  let cmp = m.cmp
  let ((l, r), b) = Dict.split(~cmp, m.data, x)
  (({cmp, data: l}, {cmp, data: r}), b)
}

let merge = (s1, s2, f) => {
  let cmp = s1.cmp
  {cmp, data: Dict.merge(~cmp, s1.data, s2.data, f)}
}

let make = (type key idx, ~id: id<key, idx>) => {
  module M = unpack(id)
  {cmp: M.cmp, data: Dict.empty}
}

let isEmpty = map => Dict.isEmpty(map.data)

let findFirstBy = (m, f) => Dict.findFirstBy(m.data, f)
let forEach = (m, f) => Dict.forEach(m.data, f)
let reduce = (m, acc, f) => Dict.reduce(m.data, acc, f)
let every = (m, f) => Dict.every(m.data, f)
let some = (m, f) => Dict.some(m.data, f)
let keep = (m, f) => {cmp: m.cmp, data: Dict.keep(m.data, f)}

let partition = (m, p) => {
  let cmp = m.cmp
  let (l, r) = m.data->Dict.partition(p)
  ({cmp, data: l}, {cmp, data: r})
}

let map = (m, f) => {cmp: m.cmp, data: Dict.map(m.data, f)}
let mapWithKey = (m, f) => {cmp: m.cmp, data: Dict.mapWithKey(m.data, f)}
let size = map => Dict.size(map.data)
let toList = map => Dict.toList(map.data)
let toArray = m => Dict.toArray(m.data)
let keysToArray = m => Dict.keysToArray(m.data)
let valuesToArray = m => Dict.valuesToArray(m.data)
let minKey = m => Dict.minKey(m.data)
let minKeyUndefined = m => Dict.minKeyUndefined(m.data)
let maxKey = m => Dict.maxKey(m.data)
let maxKeyUndefined = m => Dict.maxKeyUndefined(m.data)
let minimum = m => Dict.minimum(m.data)
let minUndefined = m => Dict.minUndefined(m.data)
let maximum = m => Dict.maximum(m.data)
let maxUndefined = m => Dict.maxUndefined(m.data)

let get = (map, x) => Dict.get(~cmp=map.cmp, map.data, x)

let getUndefined = (map, x) => Dict.getUndefined(~cmp=map.cmp, map.data, x)

let getWithDefault = (map, x, def) => Dict.getWithDefault(~cmp=map.cmp, map.data, x, def)

let getOrThrow = (map, x) => Dict.getOrThrow(~cmp=map.cmp, map.data, x)

let getExn = getOrThrow

let has = (map, x) => Dict.has(~cmp=map.cmp, map.data, x)

let checkInvariantInternal = m => Dict.checkInvariantInternal(m.data)

let eq = (m1, m2, veq) => Dict.eq(~kcmp=m1.cmp, ~veq, m1.data, m2.data)

let cmp = (m1, m2, vcmp) => Dict.cmp(~kcmp=m1.cmp, ~vcmp, m1.data, m2.data)

let getData = m => m.data

let getId = (type key identity, m: t<key, _, identity>): id<key, identity> => {
  module T = {
    type identity = identity
    type t = key
    let cmp = m.cmp
  }
  module(T)
}

let packIdData = (type key idx, ~id: id<key, idx>, ~data) => {
  module M = unpack(id)
  {cmp: M.cmp, data}
}

let cmpU = cmp
let eqU = eq
let everyU = every
let findFirstByU = findFirstBy
let forEachU = forEach
let keepU = keep
let mapU = map
let mapWithKeyU = mapWithKey
let mergeU = merge
let partitionU = partition
let reduceU = reduce
let someU = some
let updateU = update
