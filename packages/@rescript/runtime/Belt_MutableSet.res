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

module Int = Belt_MutableSetInt
module String = Belt_MutableSetString

module N = Belt_internalAVLset
module A = Belt_Array
module Sort = Belt_SortArray

type id<'k, 'id> = Belt_Id.comparable<'k, 'id>
type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

type t<'value, 'id> = {
  cmp: cmp<'value, 'id>,
  mutable data: N.t<'value>,
}

let rec remove0 = (nt, x, ~cmp) => {
  let k = nt.N.value
  let c = cmp(x, k)
  if c == 0 {
    let {N.left: l, right: r} = nt
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (Some(_), Some(nr)) =>
      nt.right = N.removeMinAuxWithRootMutate(nt, nr)
      Some(N.balMutate(nt))
    }
  } else if c < 0 {
    switch nt.left {
    | None => Some(nt)
    | Some(l) =>
      nt.left = remove0(~cmp, l, x)
      Some(N.balMutate(nt))
    }
  } else {
    switch nt.right {
    | None => Some(nt)
    | Some(r) =>
      nt.right = remove0(~cmp, r, x)
      Some(N.balMutate(nt))
    }
  }
}

let remove = (d, v) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(oldRoot2) =>
    let newRoot = remove0(~cmp=Belt_Id.getCmpInternal(d.cmp), oldRoot2, v)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
  }
}

let rec removeMany0 = (t, xs, i, len, ~cmp) =>
  if i < len {
    let ele = A.getUnsafe(xs, i)
    let u = remove0(t, ele, ~cmp)
    switch u {
    | None => None
    | Some(t) => removeMany0(t, xs, i + 1, len, ~cmp)
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
    d.data = removeMany0(nt, xs, 0, len, ~cmp=Belt_Id.getCmpInternal(d.cmp))
  }
}

let rec removeCheck0 = (nt, x, removed, ~cmp) => {
  let k = nt.N.value
  let c = Belt_Id.getCmpInternal(cmp)(x, k)
  if c == 0 {
    let () = removed.contents = true
    let {N.left: l, right: r} = nt
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (Some(_), Some(nr)) =>
      nt.right = N.removeMinAuxWithRootMutate(nt, nr)
      Some(N.balMutate(nt))
    }
  } else if c < 0 {
    switch nt.left {
    | None => Some(nt)
    | Some(l) =>
      nt.left = removeCheck0(~cmp, l, x, removed)
      Some(N.balMutate(nt))
    }
  } else {
    switch nt.right {
    | None => Some(nt)
    | Some(r) =>
      nt.right = removeCheck0(~cmp, r, x, removed)
      Some(N.balMutate(nt))
    }
  }
}

let removeCheck = (d, v) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => false
  | Some(oldRoot2) =>
    let removed = ref(false)
    let newRoot = removeCheck0(~cmp=d.cmp, oldRoot2, v, removed)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
    removed.contents
  }
}

let rec addCheck0 = (t, x, added, ~cmp) =>
  switch t {
  | None =>
    added.contents = true
    N.singleton(x)
  | Some(nt) =>
    let k = nt.N.value
    let c = cmp(x, k)
    if c == 0 {
      t
    } else {
      let {N.left: l, right: r} = nt
      if c < 0 {
        let ll = addCheck0(~cmp, l, x, added)
        nt.left = ll
      } else {
        nt.right = addCheck0(~cmp, r, x, added)
      }
      Some(N.balMutate(nt))
    }
  }

let addCheck = (m, e) => {
  let oldRoot = m.data
  let added = ref(false)
  let newRoot = addCheck0(~cmp=Belt_Id.getCmpInternal(m.cmp), oldRoot, e, added)
  if newRoot !== oldRoot {
    m.data = newRoot
  }
  added.contents
}

let add = (m, e) => {
  let oldRoot = m.data
  let newRoot = N.addMutate(~cmp=m.cmp, oldRoot, e)
  if newRoot !== oldRoot {
    m.data = newRoot
  }
}

let addArrayMutate = (t, xs, ~cmp) => {
  let v = ref(t)
  for i in 0 to A.length(xs) - 1 {
    v.contents = N.addMutate(v.contents, A.getUnsafe(xs, i), ~cmp)
  }
  v.contents
}

let mergeMany = (d, xs) => d.data = addArrayMutate(d.data, xs, ~cmp=d.cmp)

let make = (type value identity, ~id: id<value, identity>) => {
  module M = unpack(id)
  {cmp: M.cmp, data: None}
}

let isEmpty = d => N.isEmpty(d.data)

let minimum = d => N.minimum(d.data)
let minUndefined = d => N.minUndefined(d.data)
let maximum = d => N.maximum(d.data)
let maxUndefined = d => N.maxUndefined(d.data)

let forEach = (d, f) => N.forEach(d.data, f)
let reduce = (d, acc, cb) => N.reduce(d.data, acc, cb)
let every = (d, p) => N.every(d.data, p)
let some = (d, p) => N.some(d.data, p)
let size = d => N.size(d.data)
let toList = d => N.toList(d.data)
let toArray = d => N.toArray(d.data)

let fromSortedArrayUnsafe = (type value identity, xs, ~id: id<value, identity>): t<_> => {
  module M = unpack(id)
  {data: N.fromSortedArrayUnsafe(xs), cmp: M.cmp}
}

let checkInvariantInternal = d => N.checkInvariantInternal(d.data)

let fromArray = (type value identity, data, ~id: id<value, identity>) => {
  module M = unpack(id)
  let cmp = M.cmp
  {cmp, data: N.fromArray(~cmp, data)}
}

let cmp = (d0, d1) => N.cmp(~cmp=d0.cmp, d0.data, d1.data)

let eq = (d0, d1) => N.eq(~cmp=d0.cmp, d0.data, d1.data)

let get = (d, x) => N.get(~cmp=d.cmp, d.data, x)

let getUndefined = (d, x) => N.getUndefined(~cmp=d.cmp, d.data, x)

let getOrThrow = (d, x) => N.getOrThrow(~cmp=d.cmp, d.data, x)

let getExn = getOrThrow

let split = (d, key) => {
  let arr = N.toArray(d.data)
  let cmp = d.cmp
  let i = Sort.binarySearchBy(arr, key, Belt_Id.getCmpInternal(cmp))
  let len = A.length(arr)
  if i < 0 {
    let next = -i - 1
    (
      (
        {
          data: N.fromSortedArrayAux(arr, 0, next),
          cmp,
        },
        {
          data: N.fromSortedArrayAux(arr, next, len - next),
          cmp,
        },
      ),
      false,
    )
  } else {
    (
      (
        {
          data: N.fromSortedArrayAux(arr, 0, i),
          cmp,
        },
        {
          data: N.fromSortedArrayAux(arr, i + 1, len - i - 1),
          cmp,
        },
      ),
      true,
    )
  }
}

let keep = (d, p) => {data: N.keepCopy(d.data, p), cmp: d.cmp}

let partition = (d, p) => {
  let cmp = d.cmp
  let (a, b) = N.partitionCopy(d.data, p)
  ({data: a, cmp}, {data: b, cmp})
}

let subset = (a, b) => N.subset(~cmp=a.cmp, a.data, b.data)

let intersect = (a, b): t<_> => {
  let cmp = a.cmp
  switch (a.data, b.data) {
  | (None, _) => {cmp, data: None}
  | (_, None) => {cmp, data: None}
  | (Some(dataa0), Some(datab0)) =>
    let (sizea, sizeb) = (N.lengthNode(dataa0), N.lengthNode(datab0))
    let totalSize = sizea + sizeb
    let tmp = A.makeUninitializedUnsafe(totalSize)
    ignore(N.fillArray(dataa0, 0, tmp))
    ignore(N.fillArray(datab0, sizea, tmp))
    let p = Belt_Id.getCmpInternal(cmp)
    if (
      p(A.getUnsafe(tmp, sizea - 1), A.getUnsafe(tmp, sizea)) < 0 ||
        p(A.getUnsafe(tmp, totalSize - 1), A.getUnsafe(tmp, 0)) < 0
    ) {
      {cmp, data: None}
    } else {
      let tmp2 = A.makeUninitializedUnsafe(Pervasives.min(sizea, sizeb))
      let k = Sort.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, p)
      {
        data: N.fromSortedArrayAux(tmp2, 0, k),
        cmp,
      }
    }
  }
}

let diff = (a, b): t<_> => {
  let cmp = a.cmp
  let dataa = a.data
  switch (dataa, b.data) {
  | (None, _) => {cmp, data: None}
  | (_, None) => {data: N.copy(dataa), cmp}
  | (Some(dataa0), Some(datab0)) =>
    let (sizea, sizeb) = (N.lengthNode(dataa0), N.lengthNode(datab0))
    let totalSize = sizea + sizeb
    let tmp = A.makeUninitializedUnsafe(totalSize)
    ignore(N.fillArray(dataa0, 0, tmp))
    ignore(N.fillArray(datab0, sizea, tmp))
    let p = Belt_Id.getCmpInternal(cmp)
    if (
      p(A.getUnsafe(tmp, sizea - 1), A.getUnsafe(tmp, sizea)) < 0 ||
        p(A.getUnsafe(tmp, totalSize - 1), A.getUnsafe(tmp, 0)) < 0
    ) {
      {data: N.copy(dataa), cmp}
    } else {
      let tmp2 = A.makeUninitializedUnsafe(sizea)
      let k = Sort.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, p)
      {data: N.fromSortedArrayAux(tmp2, 0, k), cmp}
    }
  }
}

let union = (a, b) => {
  let cmp = a.cmp
  let (dataa, datab) = (a.data, b.data)
  switch (dataa, datab) {
  | (None, _) => {data: N.copy(datab), cmp}
  | (_, None) => {data: N.copy(dataa), cmp}
  | (Some(dataa0), Some(datab0)) =>
    let (sizea, sizeb) = (N.lengthNode(dataa0), N.lengthNode(datab0))
    let totalSize = sizea + sizeb
    let tmp = A.makeUninitializedUnsafe(totalSize)
    ignore(N.fillArray(dataa0, 0, tmp))
    ignore(N.fillArray(datab0, sizea, tmp))
    let p = Belt_Id.getCmpInternal(cmp)
    if p(A.getUnsafe(tmp, sizea - 1), A.getUnsafe(tmp, sizea)) < 0 {
      {data: N.fromSortedArrayAux(tmp, 0, totalSize), cmp}
    } else {
      let tmp2 = A.makeUninitializedUnsafe(totalSize)
      let k = Sort.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, p)
      {data: N.fromSortedArrayAux(tmp2, 0, k), cmp}
    }
  }
}

let has = (d, x) => N.has(~cmp=d.cmp, d.data, x)

let copy = d => {data: N.copy(d.data), cmp: d.cmp}

let everyU = every
let forEachU = forEach
let keepU = keep
let partitionU = partition
let reduceU = reduce
let someU = some
