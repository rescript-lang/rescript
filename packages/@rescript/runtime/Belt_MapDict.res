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

module N = Belt_internalAVLtree
module A = Belt_Array

type t<'key, 'a, 'id> = N.t<'key, 'a>

type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

let empty = None
let fromArray = N.fromArray
let isEmpty = N.isEmpty
let cmp = N.cmp
let eq = N.eq
let has = N.has
let findFirstBy = N.findFirstBy

let forEach = N.forEach
let reduce = N.reduce
let every = N.every
let some = N.some

let size = N.size
let toList = N.toList
let toArray = N.toArray
let keysToArray = N.keysToArray
let valuesToArray = N.valuesToArray

let minimum = N.minimum
let maximum = N.maximum
let minKey = N.minKey
let maxKey = N.maxKey
let minKeyUndefined = N.minKeyUndefined
let maxKeyUndefined = N.maxKeyUndefined
let minUndefined = N.minUndefined
let maxUndefined = N.maxUndefined
let get = N.get
let getUndefined = N.getUndefined
let getWithDefault = N.getWithDefault
let getOrThrow = N.getOrThrow
let getExn = getOrThrow

let mapWithKey = N.mapWithKey

let map = N.map
let keep = N.keepShared
let partition = N.partitionShared
let checkInvariantInternal = N.checkInvariantInternal
let rec set = (t: t<_>, newK, newD, ~cmp) =>
  switch t {
  | None => N.singleton(newK, newD)
  | Some(n) =>
    let k = n.N.key
    let c = Belt_Id.getCmpInternal(cmp)(newK, k)
    if c == 0 {
      Some(N.updateValue(n, newD))
    } else {
      let (l, r, v) = (n.N.left, n.N.right, n.N.value)
      if c < 0 {
        /* Worth optimize for reference equality? */
        N.bal(set(~cmp, l, newK, newD), k, v, r)
      } else {
        N.bal(l, k, v, set(~cmp, r, newK, newD))
      }
    }
  }

let rec update = (t: t<_>, newK, f, ~cmp): t<_> =>
  switch t {
  | None =>
    switch f(None) {
    | None => t
    | Some(newD) => N.singleton(newK, newD)
    }
  | Some(n) =>
    let k = n.N.key
    let c = Belt_Id.getCmpInternal(cmp)(newK, k)
    if c == 0 {
      switch f(Some(n.N.value)) {
      | None =>
        let (l, r) = (n.N.left, n.N.right)
        switch (l, r) {
        | (None, _) => r
        | (_, None) => l
        | (_, Some(rn)) =>
          let (kr, vr) = (ref(rn.key), ref(rn.value))
          let r = N.removeMinAuxWithRef(rn, kr, vr)
          N.bal(l, kr.contents, vr.contents, r)
        }
      | Some(newD) => Some(N.updateValue(n, newD))
      }
    } else {
      let (l, r, v) = (n.N.left, n.N.right, n.N.value)
      if c < 0 {
        let ll = update(~cmp, l, newK, f)
        if l === ll {
          t
        } else {
          N.bal(ll, k, v, r)
        }
      } else {
        let rr = update(~cmp, r, newK, f)
        if r === rr {
          t
        } else {
          N.bal(l, k, v, rr)
        }
      }
    }
  }

/* unboxing API was not exported
    since the correct API is really awkard
    `bool -> 'k Js.null -> ('a Js.null * bool)`
    even for specialized `k` the first `bool` can 
    be erased, maybe the perf boost does not justify the inclusion of such API

    `updateWithNull m x f`
    the callback to `f exist v` 
    when `v` is non-null,
    `exist` is guaranteed to be true
    `v` is guranteed to be `null`,
    when `exist` is `true`, `v` could be `null`, 
    since `'a` is polymorphic
*/

let rec removeAux0 = (n, x, ~cmp) => {
  let {N.left: l, key: v, right: r} = n
  let c = Belt_Id.getCmpInternal(cmp)(x, v)
  if c == 0 {
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (_, Some(rn)) =>
      let (kr, vr) = (ref(rn.key), ref(rn.value))
      let r = N.removeMinAuxWithRef(rn, kr, vr)
      N.bal(l, kr.contents, vr.contents, r)
    }
  } else if c < 0 {
    switch l {
    | None => Some(n) /* Nothing to remove */
    | Some(left) =>
      let ll = removeAux0(left, x, ~cmp)
      if ll === l {
        Some(n)
      } else {
        N.bal(ll, v, n.N.value, r)
      }
    }
  } else {
    switch r {
    | None => Some(n) /* Nothing to remove */
    | Some(right) =>
      let rr = removeAux0(~cmp, right, x)
      if rr === r {
        Some(n)
      } else {
        N.bal(l, v, n.N.value, rr)
      }
    }
  }
}

let remove = (n, x, ~cmp) =>
  switch n {
  | None => None
  | Some(n) => removeAux0(n, x, ~cmp)
  }

let mergeMany = (h, arr, ~cmp) => {
  let len = A.length(arr)
  let v = ref(h)
  for i in 0 to len - 1 {
    let (key, value) = A.getUnsafe(arr, i)
    v.contents = set(v.contents, ~cmp, key, value)
  }
  v.contents
}

let rec splitAuxPivot = (n, x, pres, ~cmp) => {
  let {N.left: l, key: v, value: d, right: r} = n
  let c = Belt_Id.getCmpInternal(cmp)(x, v)
  if c == 0 {
    pres.contents = Some(d)
    (l, r)
  } else if c < 0 {
    switch l {
    | None => (None, Some(n))
    | Some(l) =>
      let (ll, rl) = splitAuxPivot(~cmp, l, x, pres)
      (ll, N.join(rl, v, d, r))
    }
  } else {
    switch r {
    | None => (Some(n), None)
    | Some(r) =>
      let (lr, rr) = splitAuxPivot(~cmp, r, x, pres)
      (N.join(l, v, d, lr), rr)
    }
  }
}

let split = (n, x, ~cmp) =>
  switch n {
  | None => ((None, None), None)
  | Some(n) =>
    let pres = ref(None)
    let v = splitAuxPivot(~cmp, n, x, pres)
    (v, pres.contents)
  }

let rec merge = (s1, s2, f, ~cmp) =>
  switch (s1, s2) {
  | (None, None) => None
  | (Some(_), None) => N.keepMap(s1, (k, v) => f(k, Some(v), None))
  | (None, Some(_)) => N.keepMap(s2, (k, v) => f(k, None, Some(v)))
  | (Some(s1n), Some(s2n)) =>
    if s1n.height >= s2n.height {
      let {N.left: l1, key: v1, value: d1, right: r1} = s1n
      let d2 = ref(None)
      let (l2, r2) = splitAuxPivot(~cmp, s2n, v1, d2)
      let d2 = d2.contents
      let newLeft = merge(~cmp, l1, l2, f)
      let newD = f(v1, Some(d1), d2)
      let newRight = merge(~cmp, r1, r2, f)
      N.concatOrJoin(newLeft, v1, newD, newRight)
    } else {
      let {N.left: l2, key: v2, value: d2, right: r2} = s2n
      let d1 = ref(None)
      let (l1, r1) = splitAuxPivot(~cmp, s1n, v2, d1)
      let d1 = d1.contents
      let newLeft = merge(~cmp, l1, l2, f)
      let newD = f(v2, d1, Some(d2))
      let newRight = merge(~cmp, r1, r2, f)
      N.concatOrJoin(newLeft, v2, newD, newRight)
    }
  }

let rec removeMany0 = (t, xs, i, len, ~cmp) =>
  if i < len {
    let ele = A.getUnsafe(xs, i)
    let u = removeAux0(t, ele, ~cmp)
    switch u {
    | None => u
    | Some(t) => removeMany0(t, xs, i + 1, len, ~cmp)
    }
  } else {
    Some(t)
  }

let removeMany = (t, keys, ~cmp) => {
  let len = A.length(keys)
  switch t {
  | None => None
  | Some(t) => removeMany0(t, keys, 0, len, ~cmp)
  }
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
