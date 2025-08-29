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

module N = Belt_internalAVLset
module A = Belt_Array

type t<'k, 'id> = N.t<'k>

type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

/* here we relies on reference transparence
   address equality means everything equal across time
   no need to call `bal` again
*/
let rec add = (t: t<_>, x, ~cmp): t<_> =>
  switch t {
  | None => N.singleton(x)
  | Some(nt) =>
    let k = nt.value
    let c = Belt_Id.getCmpInternal(cmp)(x, k)
    if c == 0 {
      t
    } else {
      let {N.left: l, right: r} = nt
      if c < 0 {
        let ll = add(~cmp, l, x)
        if ll === l {
          t
        } else {
          N.bal(ll, k, r)
        }
      } else {
        let rr = add(~cmp, r, x)
        if rr === r {
          t
        } else {
          N.bal(l, k, rr)
        }
      }
    }
  }

let rec remove = (t: t<_>, x, ~cmp): t<_> =>
  switch t {
  | None => t
  | Some(n) =>
    let {N.left: l, value: v, right: r} = n
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      switch (l, r) {
      | (None, _) => r
      | (_, None) => l
      | (_, Some(rn)) =>
        let v = ref(rn.value)
        let r = N.removeMinAuxWithRef(rn, v)
        N.bal(l, v.contents, r)
      }
    } else if c < 0 {
      let ll = remove(~cmp, l, x)
      if ll === l {
        t
      } else {
        N.bal(ll, v, r)
      }
    } else {
      let rr = remove(~cmp, r, x)
      if rr === r {
        t
      } else {
        N.bal(l, v, rr)
      }
    }
  }

let mergeMany = (h, arr, ~cmp) => {
  let len = A.length(arr)
  let v = ref(h)
  for i in 0 to len - 1 {
    let key = A.getUnsafe(arr, i)
    v.contents = add(v.contents, ~cmp, key)
  }
  v.contents
}

let removeMany = (h, arr, ~cmp) => {
  let len = A.length(arr)
  let v = ref(h)
  for i in 0 to len - 1 {
    let key = A.getUnsafe(arr, i)
    v.contents = remove(v.contents, ~cmp, key)
  }
  v.contents
}

let rec splitAuxNoPivot = (~cmp, n: N.node<_>, x): (_, _) => {
  let {N.left: l, value: v, right: r} = n
  let c = Belt_Id.getCmpInternal(cmp)(x, v)
  if c == 0 {
    (l, r)
  } else if c < 0 {
    switch l {
    | None => (None, Some(n))
    | Some(l) =>
      let (ll, rl) = splitAuxNoPivot(~cmp, l, x)
      (ll, N.joinShared(rl, v, r))
    }
  } else {
    switch r {
    | None => (Some(n), None)
    | Some(r) =>
      let (lr, rr) = splitAuxNoPivot(~cmp, r, x)
      (N.joinShared(l, v, lr), rr)
    }
  }
}

let rec splitAuxPivot = (~cmp, n: N.node<_>, x, pres): (_, _) => {
  let {N.left: l, value: v, right: r} = n
  let c = Belt_Id.getCmpInternal(cmp)(x, v)
  if c == 0 {
    pres.contents = true
    (l, r)
  } else if c < 0 {
    switch l {
    | None => (None, Some(n))
    | Some(l) =>
      let (ll, rl) = splitAuxPivot(~cmp, l, x, pres)
      (ll, N.joinShared(rl, v, r))
    }
  } else {
    switch r {
    | None => (Some(n), None)
    | Some(r) =>
      let (lr, rr) = splitAuxPivot(~cmp, r, x, pres)
      (N.joinShared(l, v, lr), rr)
    }
  }
}

let split = (t: t<_>, x, ~cmp) =>
  switch t {
  | None => ((None, None), false)
  | Some(n) =>
    let pres = ref(false)
    let v = splitAuxPivot(~cmp, n, x, pres)
    (v, pres.contents)
  }

/* `union s1 s2`
   Use the pivot to split the smaller collection
*/
let rec union = (s1: t<_>, s2: t<_>, ~cmp): t<_> =>
  switch (s1, s2) {
  | (None, _) => s2
  | (_, None) => s1
  | (Some(n1), Some(n2)) =>
    let (h1, h2) = (n1.height, n2.height)
    if h1 >= h2 {
      if h2 == 1 {
        add(~cmp, s1, n2.value)
      } else {
        let {N.left: l1, value: v1, right: r1} = n1
        let (l2, r2) = splitAuxNoPivot(~cmp, n2, v1)
        N.joinShared(union(~cmp, l1, l2), v1, union(~cmp, r1, r2))
      }
    } else if h1 == 1 {
      add(s2, ~cmp, n1.value)
    } else {
      let {N.left: l2, value: v2, right: r2} = n2
      let (l1, r1) = splitAuxNoPivot(~cmp, n1, v2)
      N.joinShared(union(~cmp, l1, l2), v2, union(~cmp, r1, r2))
    }
  }

let rec intersect = (s1: t<_>, s2: t<_>, ~cmp) =>
  switch (s1, s2) {
  | (None, _)
  | (_, None) =>
    None
  | (Some(n1), Some(n2)) =>
    let {N.left: l1, value: v1, right: r1} = n1
    let pres = ref(false)
    let (l2, r2) = splitAuxPivot(~cmp, n2, v1, pres)
    let ll = intersect(~cmp, l1, l2)
    let rr = intersect(~cmp, r1, r2)
    if pres.contents {
      N.joinShared(ll, v1, rr)
    } else {
      N.concatShared(ll, rr)
    }
  }

let rec diff = (s1, s2, ~cmp) =>
  switch (s1, s2) {
  | (None, _)
  | (_, None) => s1
  | (Some(n1), Some(n2)) =>
    let {N.left: l1, value: v1, right: r1} = n1
    let pres = ref(false)
    let (l2, r2) = splitAuxPivot(~cmp, n2, v1, pres)
    let ll = diff(~cmp, l1, l2)
    let rr = diff(~cmp, r1, r2)
    if pres.contents {
      N.concatShared(ll, rr)
    } else {
      N.joinShared(ll, v1, rr)
    }
  }

let empty = None
let fromArray = N.fromArray
let isEmpty = N.isEmpty

let cmp = N.cmp
let eq = N.eq
let has = N.has
let forEachU = N.forEach
let forEach = N.forEach
let reduceU = N.reduce
let reduce = N.reduce
let everyU = N.every
let every = N.every
let someU = N.some
let some = N.some
let size = N.size
let toList = N.toList
let toArray = N.toArray
let minimum = N.minimum
let maximum = N.maximum
let maxUndefined = N.maxUndefined
let minUndefined = N.minUndefined
let get = N.get
let getOrThrow = N.getOrThrow
let getExn = getOrThrow
let getUndefined = N.getUndefined

let fromSortedArrayUnsafe = N.fromSortedArrayUnsafe
let subset = N.subset
let keep = N.keepShared
let keepU = N.keepShared
let partitionU = N.partitionShared
let partition = N.partitionShared

let checkInvariantInternal = N.checkInvariantInternal
