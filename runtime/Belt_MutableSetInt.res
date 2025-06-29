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

/*** This module is [`Belt.MutableSet`]() specialized with key type to be a primitive type.
    It is more efficient in general, the  API is the same with [`Belt_MutableSet`]() except its key type is fixed,
    and identity is not needed(using the built-in one)
*/

module I = Belt_internalSetInt
module S = Belt_SortArrayInt

module N = Belt_internalAVLset
module A = Belt_Array

/** The type of the set elements. */
type value = I.value

/** The type of sets. */
type t = {mutable data: I.t}

let rec remove0 = (nt, x: value) => {
  let k = nt.N.value
  if x == k {
    let {N.left: l, right: r} = nt
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (Some(_), Some(nr)) =>
      nt.right = N.removeMinAuxWithRootMutate(nt, nr)
      Some(N.balMutate(nt))
    }
  } else if x < k {
    switch nt.left {
    | None => Some(nt)
    | Some(l) =>
      nt.left = remove0(l, x)
      Some(N.balMutate(nt))
    }
  } else {
    switch nt.right {
    | None => Some(nt)
    | Some(r) =>
      nt.right = remove0(r, x)
      Some(N.balMutate(nt))
    }
  }
}

let remove = (d, v) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(oldRoot2) =>
    let newRoot = remove0(oldRoot2, v)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
  }
}

let rec removeMany0 = (t, xs, i, len) =>
  if i < len {
    let ele = A.getUnsafe(xs, i)
    let u = remove0(t, ele)
    switch u {
    | None => None
    | Some(t) => removeMany0(t, xs, i + 1, len)
    }
  } else {
    Some(t)
  }

let removeMany = (d: t, xs) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => ()
  | Some(nt) =>
    let len = A.length(xs)
    d.data = removeMany0(nt, xs, 0, len)
  }
}

let rec removeCheck0 = (nt, x: value, removed) => {
  let k = nt.N.value
  if x == k {
    let () = removed.contents = true
    let {N.left: l, right: r} = nt
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (Some(_), Some(nr)) =>
      nt.right = N.removeMinAuxWithRootMutate(nt, nr)
      Some(N.balMutate(nt))
    }
  } else if x < k {
    switch nt.left {
    | None => Some(nt)
    | Some(l) =>
      nt.left = removeCheck0(l, x, removed)
      Some(N.balMutate(nt))
    }
  } else {
    switch nt.right {
    | None => Some(nt)
    | Some(r) =>
      nt.right = removeCheck0(r, x, removed)
      Some(N.balMutate(nt))
    }
  }
}

let removeCheck = (d: t, v) => {
  let oldRoot = d.data
  switch oldRoot {
  | None => false
  | Some(oldRoot2) =>
    let removed = ref(false)
    let newRoot = removeCheck0(oldRoot2, v, removed)
    if newRoot !== oldRoot {
      d.data = newRoot
    }
    removed.contents
  }
}

let rec addCheck0 = (t, x: value, added) =>
  switch t {
  | None =>
    added.contents = true
    N.singleton(x)
  | Some(nt) =>
    let k = nt.N.value
    if x == k {
      t
    } else {
      let {N.left: l, right: r} = nt
      if x < k {
        let ll = addCheck0(l, x, added)
        nt.left = ll
      } else {
        nt.right = addCheck0(r, x, added)
      }
      Some(N.balMutate(nt))
    }
  }

let addCheck = (m: t, e) => {
  let oldRoot = m.data
  let added = ref(false)
  let newRoot = addCheck0(oldRoot, e, added)
  if newRoot !== oldRoot {
    m.data = newRoot
  }
  added.contents
}

let add = (d, k) => {
  let oldRoot = d.data
  let v = I.addMutate(oldRoot, k)
  if v !== oldRoot {
    d.data = v
  }
}

let addArrayMutate = (t, xs) => {
  let v = ref(t)
  for i in 0 to A.length(xs) - 1 {
    v.contents = I.addMutate(v.contents, A.getUnsafe(xs, i))
  }
  v.contents
}

let mergeMany = (d, arr) => d.data = addArrayMutate(d.data, arr)

let make = () => {data: None}

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

let fromSortedArrayUnsafe = xs => {data: N.fromSortedArrayUnsafe(xs)}

let checkInvariantInternal = d => N.checkInvariantInternal(d.data)

let fromArray = xs => {data: I.fromArray(xs)}

let cmp = (d0, d1) => I.cmp(d0.data, d1.data)
let eq = (d0, d1) => I.eq(d0.data, d1.data)
let get = (d, x) => I.get(d.data, x)
let getUndefined = (d, x) => I.getUndefined(d.data, x)
let getOrThrow = (d, x) => I.getOrThrow(d.data, x)
let getExn = getOrThrow

let split = (d, key) => {
  let arr = N.toArray(d.data)
  let i = S.binarySearch(arr, key)
  let len = A.length(arr)
  if i < 0 {
    let next = -i - 1
    (
      (
        {data: N.fromSortedArrayAux(arr, 0, next)},
        {data: N.fromSortedArrayAux(arr, next, len - next)},
      ),
      false,
    )
  } else {
    (
      (
        {data: N.fromSortedArrayAux(arr, 0, i)},
        {data: N.fromSortedArrayAux(arr, i + 1, len - i - 1)},
      ),
      true,
    )
  }
}

let keep = (d, p) => {data: N.keepCopy(d.data, p)}

let partition = (d, p) => {
  let (a, b) = N.partitionCopy(d.data, p)
  ({data: a}, {data: b})
}

let subset = (a, b) => I.subset(a.data, b.data)
let intersect = (dataa, datab) => {
  let (dataa, datab) = (dataa.data, datab.data)
  switch (dataa, datab) {
  | (None, _) => make()
  | (_, None) => make()
  | (Some(dataa0), Some(datab0)) =>
    let (sizea, sizeb) = (N.lengthNode(dataa0), N.lengthNode(datab0))
    let totalSize = sizea + sizeb
    let tmp = A.makeUninitializedUnsafe(totalSize)
    ignore(N.fillArray(dataa0, 0, tmp))
    ignore(N.fillArray(datab0, sizea, tmp))
    if (
      A.getUnsafe(tmp, sizea - 1) < A.getUnsafe(tmp, sizea) ||
        A.getUnsafe(tmp, totalSize - 1) < A.getUnsafe(tmp, 0)
    ) {
      make()
    } else {
      let tmp2 = A.makeUninitializedUnsafe(Pervasives.min(sizea, sizeb))
      let k = S.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0)
      {data: N.fromSortedArrayAux(tmp2, 0, k)}
    }
  }
}

let diff = (dataa, datab): t => {
  let (dataa, datab) = (dataa.data, datab.data)
  switch (dataa, datab) {
  | (None, _) => make()
  | (_, None) => {data: N.copy(dataa)}
  | (Some(dataa0), Some(datab0)) =>
    let (sizea, sizeb) = (N.lengthNode(dataa0), N.lengthNode(datab0))
    let totalSize = sizea + sizeb
    let tmp = A.makeUninitializedUnsafe(totalSize)
    ignore(N.fillArray(dataa0, 0, tmp))
    ignore(N.fillArray(datab0, sizea, tmp))
    if (
      A.getUnsafe(tmp, sizea - 1) < A.getUnsafe(tmp, sizea) ||
        A.getUnsafe(tmp, totalSize - 1) < A.getUnsafe(tmp, 0)
    ) {
      {data: N.copy(dataa)}
    } else {
      let tmp2 = A.makeUninitializedUnsafe(sizea)
      let k = S.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0)
      {data: N.fromSortedArrayAux(tmp2, 0, k)}
    }
  }
}

let union = (dataa: t, datab: t): t => {
  let (dataa, datab) = (dataa.data, datab.data)
  switch (dataa, datab) {
  | (None, _) => {data: N.copy(datab)}
  | (_, None) => {data: N.copy(dataa)}
  | (Some(dataa0), Some(datab0)) =>
    let (sizea, sizeb) = (N.lengthNode(dataa0), N.lengthNode(datab0))
    let totalSize = sizea + sizeb
    let tmp = A.makeUninitializedUnsafe(totalSize)
    ignore(N.fillArray(dataa0, 0, tmp))
    ignore(N.fillArray(datab0, sizea, tmp))
    if A.getUnsafe(tmp, sizea - 1) < A.getUnsafe(tmp, sizea) {
      {data: N.fromSortedArrayAux(tmp, 0, totalSize)}
    } else {
      let tmp2 = A.makeUninitializedUnsafe(totalSize)
      let k = S.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0)
      {data: N.fromSortedArrayAux(tmp2, 0, k)}
    }
  }
}

let has = (d, x) => I.has(d.data, x)

let copy = d => {data: N.copy(d.data)}

let everyU = every
let forEachU = forEach
let keepU = keep
let partitionU = partition
let reduceU = reduce
let someU = some
