/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

module Int = Belt_MutableMapInt
module String = Belt_MutableMapString

module N = Belt_internalAVLtree
module A = Belt_Array

type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

type t<'k, 'v, 'id> = {
  cmp: cmp<'k, 'id>,
  mutable data: N.t<'k, 'v>,
}

let rec removeMutateAux = (nt, x, ~cmp) => {
  let k = nt.N.key
  let c = Belt_Id.getCmpInternal(cmp)(x, k)
  if c == 0 {
    let {N.left: l, right: r} = nt
    switch (l, r) {
    | (Some(_), Some(nr)) =>
      nt.right = N.removeMinAuxWithRootMutate(nt, nr)
      Some(N.balMutate(nt))
    | (None, Some(_)) => r
    | (Some(_) | None, None) => l
    }
  } else if c < 0 {
    switch nt.N.left {
    | None => Some(nt)
    | Some(l) =>
      nt.left = removeMutateAux(~cmp, l, x)
      Some(N.balMutate(nt))
    }
  } else {
    switch nt.right {
    | None => Some(nt)
    | Some(r) =>
      nt.right = removeMutateAux(~cmp, r, x)
      Some(N.balMutate(nt))
    }
  }
}

let remove = (d, k) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(oldRoot2) =>
    let newRoot = removeMutateAux(~cmp=d.cmp, oldRoot2, k)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
  }
}

let rec removeArrayMutateAux = (t, xs, i, len, ~cmp) =>
  if i < len {
    let ele = A.getUnsafe(xs, i)
    let u = removeMutateAux(t, ele, ~cmp)
    switch u {
    | None => None
    | Some(t) => removeArrayMutateAux(t, xs, i + 1, len, ~cmp)
    }
  } else {
    Some(t)
  }

let removeMany = (d, xs) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(nt) =>
    let len = A.length(xs)
    let newRoot = removeArrayMutateAux(nt, xs, 0, len, ~cmp=d.cmp)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
  }
}

let rec updateDone = (t, x, f, ~cmp) =>
  switch t {
  | None =>
    switch f(None) {
    | Some(data) => N.singleton(x, data)
    | None => t
    }
  | Some(nt) =>
    let k = nt.N.key
    let c = Belt_Id.getCmpInternal(cmp)(x, k)
    if c == 0 {
      switch f(Some(nt.value)) {
      | None =>
        let {N.left: l, right: r} = nt
        switch (l, r) {
        | (Some(_), Some(nr)) =>
          nt.right = N.removeMinAuxWithRootMutate(nt, nr)
          Some(N.balMutate(nt))
        | (None, Some(_)) => r
        | (Some(_) | None, None) => l
        }
      | Some(data) =>
        nt.value = data
        Some(nt)
      }
    } else {
      if c < 0 {
        nt.left = updateDone(nt.left, x, f, ~cmp)
      } else {
        nt.right = updateDone(nt.right, x, f, ~cmp)
      }
      Some(N.balMutate(nt))
    }
  }
let update = (t, x, f) => {
  let oldRoot = t.data
  let newRoot = updateDone(oldRoot, x, f, ~cmp=t.cmp)
  if newRoot !== oldRoot {
    t.data = newRoot
  }
}

let make = (type key identity, ~id: id<key, identity>) => {
  module M = unpack(id)
  {cmp: M.cmp, data: None}
}

let clear = m => m.data = None

let isEmpty = d => N.isEmpty(d.data)

let minKey = m => N.minKey(m.data)
let minKeyUndefined = m => N.minKeyUndefined(m.data)
let maxKey = m => N.maxKey(m.data)
let maxKeyUndefined = m => N.maxKeyUndefined(m.data)
let minimum = m => N.minimum(m.data)
let minUndefined = m => N.minUndefined(m.data)
let maximum = m => N.maximum(m.data)
let maxUndefined = m => N.maxUndefined(m.data)

let forEach = (d, f) => N.forEach(d.data, f)
let reduce = (d, acc, cb) => N.reduce(d.data, acc, cb)
let every = (d, p) => N.every(d.data, p)
let some = (d, p) => N.some(d.data, p)
let size = d => N.size(d.data)
let toList = d => N.toList(d.data)
let toArray = d => N.toArray(d.data)
let keysToArray = d => N.keysToArray(d.data)
let valuesToArray = d => N.valuesToArray(d.data)

/* let fromSortedArrayUnsafe (type key) (type identity) ~(id : (key,identity) id) xs : _ t =
  let module M = (val id) in 
  S.t ~data:(N.fromSortedArrayUnsafe xs) ~cmp:M.cmp */

let checkInvariantInternal = d => N.checkInvariantInternal(d.data)

let cmp = (m1, m2, cmp) => N.cmp(~kcmp=m1.cmp, ~vcmp=cmp, m1.data, m2.data)

let eq = (m1, m2, cmp) => N.eq(~kcmp=m1.cmp, ~veq=cmp, m1.data, m2.data)

let map = (m, f) => {cmp: m.cmp, data: N.map(m.data, f)}
let mapWithKey = (m, f) => {cmp: m.cmp, data: N.mapWithKey(m.data, f)}
let get = (m, x) => N.get(~cmp=m.cmp, m.data, x)

let getUndefined = (m, x) => N.getUndefined(~cmp=m.cmp, m.data, x)

let getWithDefault = (m, x, def) => N.getWithDefault(~cmp=m.cmp, m.data, x, def)

let getOrThrow = (m, x) => N.getOrThrow(~cmp=m.cmp, m.data, x)

let getExn = getOrThrow

let has = (m, x) => N.has(~cmp=m.cmp, m.data, x)

let fromArray = (type k identity, data, ~id: id<k, identity>) => {
  module M = unpack(id)
  let cmp = M.cmp
  {cmp, data: N.fromArray(~cmp, data)}
}

let set = (m, e, v) => {
  let oldRoot = m.data
  let newRoot = N.updateMutate(~cmp=m.cmp, oldRoot, e, v)
  if newRoot !== oldRoot {
    m.data = newRoot
  }
}

let mergeManyAux = (t, xs, ~cmp) => {
  let v = ref(t)
  for i in 0 to A.length(xs) - 1 {
    let (key, value) = A.getUnsafe(xs, i)
    v.contents = N.updateMutate(v.contents, key, value, ~cmp)
  }
  v.contents
}

let mergeMany = (d, xs) => {
  let oldRoot = d.data
  let newRoot = mergeManyAux(oldRoot, xs, ~cmp=d.cmp)
  if newRoot !== oldRoot {
    d.data = newRoot
  }
}

let cmpU = cmp
let eqU = eq
let everyU = every
let forEachU = forEach
let mapU = map
let mapWithKeyU = mapWithKey
let reduceU = reduce
let someU = some
let updateU = update
