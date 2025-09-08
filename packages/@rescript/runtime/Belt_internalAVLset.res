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
@@config({flags: ["-bs-noassertfalse"]})
type rec node<'value> = {
  @as("v") mutable value: 'value,
  @as("h") mutable height: int,
  @as("l") mutable left: t<'value>,
  @as("r") mutable right: t<'value>,
}
and t<'value> = option<node<'value>>

module A = Belt_Array
module S = Belt_SortArray

type cmp<'a, 'b> = Belt_Id.cmp<'a, 'b>

/* Sets are represented by balanced binary trees (the heights of the
 children differ by at most 2 */

@inline
let height = (n: t<_>) =>
  switch n {
  | None => 0
  | Some(n) => n.height
  }

let rec copy = n =>
  switch n {
  | None => n
  | Some(n) =>
    Some({
      left: copy(n.left),
      right: copy(n.right),
      value: n.value,
      height: n.height,
    })
  }

/* Creates a new node with leftGet son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | treeHeight l - treeHeight r | <= 2.
   Inline expansion of treeHeight for better speed. */

@inline
let calcHeight = (hl: int, hr) =>
  if hl >= hr {
    hl
  } else {
    hr
  } + 1

let create = (l: t<_>, v, r: t<_>) => {
  let hl = height(l)
  let hr = height(r)
  Some({left: l, value: v, right: r, height: calcHeight(hl, hr)})
}

let singleton = x => Some({left: None, value: x, right: None, height: 1})

let heightGe = (l, r) =>
  switch (l, r) {
  | (_, None) => true
  | (Some(hl), Some(hr)) => hl.height >= hr.height
  | (None, Some(_)) => false
  }
/* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | treeHeight l - treeHeight r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. */
/* TODO: inline all `create` operation, save duplicated `heightGet` calcuation */
let bal = (l, v, r) => {
  let (hl, hr) = (height(l), height(r))
  if hl > hr + 2 {
    switch l {
    | None => assert(false)
    | Some({left: ll, right: lr} as l) =>
      if heightGe(ll, lr) {
        create(ll, l.value, create(lr, v, r))
      } else {
        switch lr {
        | None => assert(false)
        | Some(lr) => create(create(ll, l.value, lr.left), lr.value, create(lr.right, v, r))
        }
      }
    }
  } else if hr > hl + 2 {
    switch r {
    | None => assert(false)
    | Some({left: rl, right: rr} as r) =>
      if heightGe(rr, rl) {
        create(create(l, v, rl), r.value, rr)
      } else {
        switch rl {
        | None => assert(false)
        | Some(rl) => create(create(l, v, rl.left), rl.value, create(rl.right, r.value, rr))
        }
      }
    }
  } else {
    Some({left: l, value: v, right: r, height: calcHeight(hl, hr)})
  }
}

let rec min0Aux = n =>
  switch n.left {
  | None => n.value
  | Some(n) => min0Aux(n)
  }

let minimum = n =>
  switch n {
  | None => None
  | Some(n) => Some(min0Aux(n))
  }

let minUndefined = n =>
  switch n {
  | None => Js.undefined
  | Some(n) => Js.Undefined.return(min0Aux(n))
  }

let rec max0Aux = n =>
  switch n.right {
  | None => n.value
  | Some(n) => max0Aux(n)
  }

let maximum = n =>
  switch n {
  | None => None
  | Some(n) => Some(max0Aux(n))
  }

let maxUndefined = n =>
  switch n {
  | None => Js.undefined
  | Some(n) => Js.Undefined.return(max0Aux(n))
  }

let rec removeMinAuxWithRef = (n, v) =>
  switch n.left {
  | None =>
    v.contents = n.value
    n.right
  | Some(ln) => bal(removeMinAuxWithRef(ln, v), n.value, n.right)
  }

/* Implementation of the set operations */

let isEmpty = n =>
  switch n {
  | Some(_) => false
  | None => true
  }

let rec stackAllLeft = (v, s) =>
  switch v {
  | None => s
  | Some(x) => stackAllLeft(x.left, list{x, ...s})
  }

let rec forEach = (n, f) =>
  switch n {
  | None => ()
  | Some(n) =>
    forEach(n.left, f)
    f(n.value)
    forEach(n.right, f)
  }

let rec reduce = (s, accu, f) =>
  switch s {
  | None => accu
  | Some(n) => reduce(n.right, f(reduce(n.left, accu, f), n.value), f)
  }

let rec every = (n, p) =>
  switch n {
  | None => true
  | Some(n) => p(n.value) && (n.left->every(p) && n.right->every(p))
  }

let rec some = (n, p) =>
  switch n {
  | None => false
  | Some(n) => p(n.value) || (some(n.left, p) || some(n.right, p))
  }

/* `addMinElement v n` and `addMaxElement v n`
   assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree.
   They are only used during the "join" operation which
   respects this precondition.
*/

let rec addMinElement = (n, v) =>
  switch n {
  | None => singleton(v)
  | Some(n) => bal(addMinElement(n.left, v), n.value, n.right)
  }

let rec addMaxElement = (n, v) =>
  switch n {
  | None => singleton(v)
  | Some(n) => bal(n.left, n.value, addMaxElement(n.right, v))
  }

/* `join ln v rn` Some a balanced tree simliar to `create ln v rn`
   bal, but no assumptions are made on the
   relative heights of `ln` and `rn`. */

let rec joinShared = (ln, v, rn) =>
  switch (ln, rn) {
  | (None, _) => addMinElement(rn, v)
  | (_, None) => addMaxElement(ln, v)
  | (Some(l), Some(r)) =>
    let lh = l.height
    let rh = r.height
    if lh > rh + 2 {
      bal(l.left, l.value, joinShared(l.right, v, rn))
    } else if rh > lh + 2 {
      bal(joinShared(ln, v, r.left), r.value, r.right)
    } else {
      create(ln, v, rn)
    }
  }

/* `concat l r`
 No assumption on the heights of l and r. */

let concatShared = (t1, t2) =>
  switch (t1, t2) {
  | (None, _) => t2
  | (_, None) => t1
  | (_, Some(t2n)) =>
    let v = ref(t2n.value)
    let t2r = removeMinAuxWithRef(t2n, v)
    joinShared(t1, v.contents, t2r)
  }

let rec partitionShared = (n, p) =>
  switch n {
  | None => (None, None)
  | Some(n) =>
    let value = n.value
    let (lt, lf) = partitionShared(n.left, p)
    let pv = p(value)
    let (rt, rf) = partitionShared(n.right, p)
    if pv {
      (joinShared(lt, value, rt), concatShared(lf, rf))
    } else {
      (concatShared(lt, rt), joinShared(lf, value, rf))
    }
  }

let rec lengthNode = n => {
  let {left: l, right: r} = n
  let sizeL = switch l {
  | None => 0
  | Some(l) => lengthNode(l)
  }
  let sizeR = switch r {
  | None => 0
  | Some(r) => lengthNode(r)
  }
  1 + sizeL + sizeR
}

let size = n =>
  switch n {
  | None => 0
  | Some(n) => lengthNode(n)
  }

let rec toListAux = (n, accu) =>
  switch n {
  | None => accu
  | Some(n) => toListAux(n.left, list{n.value, ...toListAux(n.right, accu)})
  }

let toList = s => toListAux(s, list{})

let rec checkInvariantInternal = (v: t<_>) =>
  switch v {
  | None => ()
  | Some(n) =>
    let {left: l, right: r} = n
    let diff = height(l) - height(r)
    assert(diff <= 2 && diff >= -2)
    checkInvariantInternal(l)
    checkInvariantInternal(r)
  }

let rec fillArray = (n, i, arr) => {
  let {left: l, value: v, right: r} = n
  let next = switch l {
  | None => i
  | Some(l) => fillArray(l, i, arr)
  }
  A.setUnsafe(arr, next, v)
  let rnext = next + 1
  switch r {
  | None => rnext
  | Some(r) => fillArray(r, rnext, arr)
  }
}

type cursor = {mutable forward: int, mutable backward: int}

let rec fillArrayWithPartition = (n, cursor, arr, p) => {
  let {left: l, value: v, right: r} = n
  switch l {
  | None => ()
  | Some(l) => fillArrayWithPartition(l, cursor, arr, p)
  }
  if p(v) {
    let c = cursor.forward
    A.setUnsafe(arr, c, v)
    cursor.forward = c + 1
  } else {
    let c = cursor.backward
    A.setUnsafe(arr, c, v)
    cursor.backward = c - 1
  }
  switch r {
  | None => ()
  | Some(r) => fillArrayWithPartition(r, cursor, arr, p)
  }
}

let rec fillArrayWithFilter = (n, i, arr, p) => {
  let {left: l, value: v, right: r} = n
  let next = switch l {
  | None => i
  | Some(l) => fillArrayWithFilter(l, i, arr, p)
  }
  let rnext = if p(v) {
    A.setUnsafe(arr, next, v)
    next + 1
  } else {
    next
  }
  switch r {
  | None => rnext
  | Some(r) => fillArrayWithFilter(r, rnext, arr, p)
  }
}

let toArray = n =>
  switch n {
  | None => []
  | Some(n) =>
    let size = lengthNode(n)
    let v = A.makeUninitializedUnsafe(size)
    ignore((fillArray(n, 0, v): int)) /* may add assertion */
    v
  }

let rec fromSortedArrayRevAux = (arr, off, len) =>
  switch len {
  | 0 => None
  | 1 => singleton(A.getUnsafe(arr, off))
  | 2 =>
    let (x0, x1) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off - 1))
    }

    Some({left: singleton(x0), value: x1, height: 2, right: None})
  | 3 =>
    let (x0, x1, x2) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off - 1), getUnsafe(arr, off - 2))
    }
    Some({
      left: singleton(x0),
      right: singleton(x2),
      value: x1,
      height: 2,
    })
  | _ =>
    let nl = len / 2
    let left = fromSortedArrayRevAux(arr, off, nl)
    let mid = A.getUnsafe(arr, off - nl)
    let right = fromSortedArrayRevAux(arr, off - nl - 1, len - nl - 1)
    create(left, mid, right)
  }

let rec fromSortedArrayAux = (arr, off, len) =>
  switch len {
  | 0 => None
  | 1 => singleton(A.getUnsafe(arr, off))
  | 2 =>
    let (x0, x1) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off + 1))
    }

    Some({left: singleton(x0), value: x1, height: 2, right: None})
  | 3 =>
    let (x0, x1, x2) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off + 1), getUnsafe(arr, off + 2))
    }
    Some({
      left: singleton(x0),
      right: singleton(x2),
      value: x1,
      height: 2,
    })
  | _ =>
    let nl = len / 2
    let left = fromSortedArrayAux(arr, off, nl)
    let mid = A.getUnsafe(arr, off + nl)
    let right = fromSortedArrayAux(arr, off + nl + 1, len - nl - 1)
    create(left, mid, right)
  }

let fromSortedArrayUnsafe = arr => fromSortedArrayAux(arr, 0, A.length(arr))

let rec keepShared = (n, p) =>
  switch n {
  | None => None
  | Some(n) =>
    let {left: l, value: v, right: r} = n
    let newL = keepShared(l, p)
    let pv = p(v)
    let newR = keepShared(r, p)
    if pv {
      if l === newL && r === newR {
        Some(n)
      } else {
        joinShared(newL, v, newR)
      }
    } else {
      concatShared(newL, newR)
    }
  }

/* ATT: functional methods in general can be shared with
    imperative methods, however, it does not apply when functional
    methods makes use of referential equality
*/

let keepCopy = (n, p): t<_> =>
  switch n {
  | None => None
  | Some(n) =>
    let size = lengthNode(n)
    let v = A.makeUninitializedUnsafe(size)
    let last = fillArrayWithFilter(n, 0, v, p)
    fromSortedArrayAux(v, 0, last)
  }

let partitionCopy = (n, p) =>
  switch n {
  | None => (None, None)
  | Some(n) =>
    let size = lengthNode(n)
    let v = A.makeUninitializedUnsafe(size)
    let backward = size - 1
    let cursor = {forward: 0, backward}
    fillArrayWithPartition(n, cursor, v, p)
    let forwardLen = cursor.forward
    (fromSortedArrayAux(v, 0, forwardLen), fromSortedArrayRevAux(v, backward, size - forwardLen))
  }

let rec has = (t: t<_>, x, ~cmp) =>
  switch t {
  | None => false
  | Some(n) =>
    let v = n.value
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    c == 0 ||
      has(
        ~cmp,
        if c < 0 {
          n.left
        } else {
          n.right
        },
        x,
      )
  }

let rec compareAux = (e1, e2, ~cmp) =>
  switch (e1, e2) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let c = Belt_Id.getCmpInternal(cmp)(h1.value, h2.value)
    if c == 0 {
      compareAux(~cmp, h1.right->stackAllLeft(t1), h2.right->stackAllLeft(t2))
    } else {
      c
    }
  | (_, _) => 0
  }

let cmp = (s1, s2, ~cmp) => {
  let (len1, len2) = (size(s1), size(s2))
  if len1 == len2 {
    compareAux(~cmp, stackAllLeft(s1, list{}), stackAllLeft(s2, list{}))
  } else if len1 < len2 {
    -1
  } else {
    1
  }
}

let eq = (s1, s2, ~cmp as c) => cmp(~cmp=c, s1, s2) == 0

let rec subset = (s1: t<_>, s2: t<_>, ~cmp) =>
  switch (s1, s2) {
  | (None, _) => true
  | (_, None) => false
  | (Some(t1), Some(t2)) =>
    let {left: l1, value: v1, right: r1} = t1
    let {left: l2, value: v2, right: r2} = t2
    let c = Belt_Id.getCmpInternal(cmp)(v1, v2)
    if c == 0 {
      subset(~cmp, l1, l2) && subset(~cmp, r1, r2)
    } else if c < 0 {
      subset(~cmp, create(l1, v1, None), l2) && subset(~cmp, r1, s2)
    } else {
      subset(~cmp, create(None, v1, r1), r2) && subset(~cmp, l1, s2)
    }
  }

let rec get = (n: t<_>, x, ~cmp) =>
  switch n {
  | None => None
  | Some(t) /* Node(l, v, r, _) */ =>
    let v = t.value
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      Some(v)
    } else {
      get(
        ~cmp,
        if c < 0 {
          t.left
        } else {
          t.right
        },
        x,
      )
    }
  }

let rec getUndefined = (n: t<_>, x, ~cmp) =>
  switch n {
  | None => Js.Undefined.empty
  | Some(t) /* Node(l, v, r, _) */ =>
    let v = t.value
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      Js.Undefined.return(v)
    } else {
      getUndefined(
        ~cmp,
        if c < 0 {
          t.left
        } else {
          t.right
        },
        x,
      )
    }
  }

let rec getOrThrow = (n: t<_>, x, ~cmp) =>
  switch n {
  | None => throw(Not_found)
  | Some(t) /* Node(l, v, r, _) */ =>
    let v = t.value
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      v
    } else {
      getOrThrow(
        ~cmp,
        if c < 0 {
          t.left
        } else {
          t.right
        },
        x,
      )
    }
  }

/* **************************************************************** */

/*
  L rotation, Some root node
*/
let rotateWithLeftChild = k2 =>
  switch k2.left {
  | None => assert(false)
  | Some(k1) =>
    k2.left = k1.right
    k1.right = Some(k2)
    let (hlk2, hrk2) = (k2.left->height, k2.right->height)
    k2.height = Pervasives.max(hlk2, hrk2) + 1
    let (hlk1, hk2) = (k1.left->height, k2.height)
    k1.height = Pervasives.max(hlk1, hk2) + 1
    k1
  }
/* right rotation */
let rotateWithRightChild = k1 =>
  switch k1.right {
  | None => assert(false)
  | Some(k2) =>
    k1.right = k2.left
    k2.left = Some(k1)
    let (hlk1, hrk1) = (k1.left->height, k1.right->height)
    k1.height = Pervasives.max(hlk1, hrk1) + 1
    let (hrk2, hk1) = (k2.right->height, k1.height)
    k2.height = Pervasives.max(hrk2, hk1) + 1
    k2
  }

/*
  double l rotation
*/
/** */
let doubleWithLeftChild = k3 =>
  switch k3.left {
  | None => assert(false)
  | Some(k3l) =>
    let v = k3l->rotateWithRightChild->Some
    k3.left = v
    k3->rotateWithLeftChild
  }

let doubleWithRightChild = k2 =>
  switch k2.right {
  | None => assert(false)
  | Some(k2r) =>
    let v = k2r->rotateWithLeftChild->Some
    k2.right = v
    rotateWithRightChild(k2)
  }

let heightUpdateMutate = t => {
  let (hlt, hrt) = (t.left->height, t.right->height)
  t.height = Pervasives.max(hlt, hrt) + 1
  t
}

let balMutate = nt => {
  let {left: l, right: r} = nt
  let (hl, hr) = (height(l), height(r))
  if hl > 2 + hr {
    switch l {
    | None => assert(false)
    | Some({left: ll, right: lr}) =>
      if heightGe(ll, lr) {
        heightUpdateMutate(rotateWithLeftChild(nt))
      } else {
        heightUpdateMutate(doubleWithLeftChild(nt))
      }
    }
  } else if hr > 2 + hl {
    switch r {
    | None => assert(false)
    | Some({left: rl, right: rr}) =>
      if heightGe(rr, rl) {
        heightUpdateMutate(rotateWithRightChild(nt))
      } else {
        heightUpdateMutate(doubleWithRightChild(nt))
      }
    }
  } else {
    nt.height = Pervasives.max(hl, hr) + 1
    nt
  }
}

let rec addMutate = (~cmp, t: t<_>, x) =>
  switch t {
  | None => singleton(x)
  | Some(nt) =>
    let k = nt.value
    let c = Belt_Id.getCmpInternal(cmp)(x, k)
    if c == 0 {
      t
    } else {
      let {left: l, right: r} = nt
      if c < 0 {
        let ll = addMutate(~cmp, l, x)
        nt.left = ll
      } else {
        nt.right = addMutate(~cmp, r, x)
      }
      Some(balMutate(nt))
    }
  }

let fromArray = (xs: array<_>, ~cmp) => {
  let len = A.length(xs)
  if len == 0 {
    None
  } else {
    let next = ref(S.strictlySortedLength(xs, (x, y) => Belt_Id.getCmpInternal(cmp)(x, y) < 0))
    let result = ref(
      if next.contents >= 0 {
        fromSortedArrayAux(xs, 0, next.contents)
      } else {
        next.contents = -next.contents
        fromSortedArrayRevAux(xs, next.contents - 1, next.contents)
      },
    )
    for i in next.contents to len - 1 {
      result.contents = addMutate(~cmp, result.contents, A.getUnsafe(xs, i))
    }
    result.contents
  }
}

let rec removeMinAuxWithRootMutate = (nt, n) => {
  let {right: rn, left: ln} = n
  switch ln {
  | None =>
    nt.value = n.value
    rn
  | Some(ln) =>
    n.left = removeMinAuxWithRootMutate(nt, ln)
    Some(balMutate(n))
  }
}
