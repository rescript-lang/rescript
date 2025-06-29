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
/* ********************************************************************* */

/* Almost rewritten  by authors of ReScript */

@@config({flags: ["-bs-noassertfalse"]})
type rec node<'k, 'v> = {
  @as("k") mutable key: 'k,
  @as("v") mutable value: 'v,
  @as("h") mutable height: int,
  @as("l") mutable left: t<'k, 'v>,
  @as("r") mutable right: t<'k, 'v>,
}
and t<'key, 'a> = option<node<'key, 'a>>

type cmp<'k, 'id> = Belt_Id.cmp<'k, 'id>

module A = Belt_Array
module S = Belt_SortArray

let treeHeight = (n: t<_>) =>
  switch n {
  | None => 0
  | Some(n) => n.height
  }

let rec copy = n =>
  switch n {
  | None => n
  | Some(n) => Some({...n, left: copy(n.left), right: copy(n.right)})
  }

let create = (l, x, d, r) => {
  let (hl, hr) = (treeHeight(l), treeHeight(r))
  Some({
    left: l,
    key: x,
    value: d,
    right: r,
    height: if hl >= hr {
      hl + 1
    } else {
      hr + 1
    },
  })
}

let singleton = (x, d) => Some({left: None, key: x, value: d, right: None, height: 1})

let heightGe = (l, r) =>
  switch (l, r) {
  | (_, None) => true
  | (Some(hl), Some(hr)) => hl.height >= hr.height
  | (None, Some(_)) => false
  }

let updateValue = (n, newValue) =>
  if n.value === newValue {
    n
  } else {
    {
      left: n.left,
      right: n.right,
      key: n.key,
      value: newValue,
      height: n.height,
    }
  }

let bal = (l, x, d, r) => {
  let hl = switch l {
  | None => 0
  | Some(n) => n.height
  }
  let hr = switch r {
  | None => 0
  | Some(n) => n.height
  }
  if hl > hr + 2 {
    switch l {
    | None => assert(false)
    | Some({left: ll, right: lr} as l) =>
      if treeHeight(ll) >= treeHeight(lr) {
        create(ll, l.key, l.value, create(lr, x, d, r))
      } else {
        switch lr {
        | None => assert(false)
        | Some(lr) =>
          create(create(ll, l.key, l.value, lr.left), lr.key, lr.value, create(lr.right, x, d, r))
        }
      }
    }
  } else if hr > hl + 2 {
    switch r {
    | None => assert(false)
    | Some({left: rl, right: rr} as r) =>
      if treeHeight(rr) >= treeHeight(rl) {
        create(create(l, x, d, rl), r.key, r.value, rr)
      } else {
        switch rl {
        | None => assert(false)
        | Some(rl) =>
          create(create(l, x, d, rl.left), rl.key, rl.value, create(rl.right, r.key, r.value, rr))
        }
      }
    }
  } else {
    Some({
      left: l,
      key: x,
      value: d,
      right: r,
      height: if hl >= hr {
        hl + 1
      } else {
        hr + 1
      },
    })
  }
}

let rec minKey0Aux = n =>
  switch n.left {
  | None => n.key
  | Some(n) => minKey0Aux(n)
  }

let minKey = n =>
  switch n {
  | None => None
  | Some(n) => Some(minKey0Aux(n))
  }

let minKeyUndefined = n =>
  switch n {
  | None => Js.undefined
  | Some(n) => Js.Undefined.return(minKey0Aux(n))
  }

let rec maxKey0Aux = n =>
  switch n.right {
  | None => n.key
  | Some(n) => maxKey0Aux(n)
  }

let maxKey = n =>
  switch n {
  | None => None
  | Some(n) => Some(maxKey0Aux(n))
  }

let maxKeyUndefined = n =>
  switch n {
  | None => Js.undefined
  | Some(n) => Js.Undefined.return(maxKey0Aux(n))
  }

let rec minKV0Aux = n =>
  switch n.left {
  | None => (n.key, n.value)
  | Some(n) => minKV0Aux(n)
  }

let minimum = n =>
  switch n {
  | None => None
  | Some(n) => Some(minKV0Aux(n))
  }

let minUndefined = n =>
  switch n {
  | None => Js.undefined
  | Some(n) => Js.Undefined.return(minKV0Aux(n))
  }

let rec maxKV0Aux = n =>
  switch n.right {
  | None => (n.key, n.value)
  | Some(n) => maxKV0Aux(n)
  }

let maximum = n =>
  switch n {
  | None => None
  | Some(n) => Some(maxKV0Aux(n))
  }

let maxUndefined = n =>
  switch n {
  | None => Js.undefined
  | Some(n) => Js.Undefined.return(maxKV0Aux(n))
  }

/* TODO: use kv ref */
let rec removeMinAuxWithRef = (n, kr, vr) =>
  switch n.left {
  | None =>
    kr.contents = n.key
    vr.contents = n.value
    n.right
  | Some(ln) => bal(removeMinAuxWithRef(ln, kr, vr), n.key, n.value, n.right)
  }

let isEmpty = x =>
  switch x {
  | None => true
  | Some(_) => false
  }

let rec stackAllLeft = (v, s) =>
  switch v {
  | None => s
  | Some(x) => stackAllLeft(x.left, list{x, ...s})
  }

let rec findFirstBy = (n, p) =>
  switch n {
  | None => None
  | Some(n) =>
    let left = findFirstBy(n.left, p)
    if left != None {
      left
    } else {
      let {key: v, value: d} = n
      let pvd = p(v, d)
      if pvd {
        Some(v, d)
      } else {
        let right = findFirstBy(n.right, p)
        if right != None {
          right
        } else {
          None
        }
      }
    }
  }

let rec forEach = (n, f) =>
  switch n {
  | None => ()
  | Some(n) =>
    forEach(n.left, f)
    f(n.key, n.value)
    forEach(n.right, f)
  }

let rec map = (n, f) =>
  switch n {
  | None => None
  | Some(n) =>
    let newLeft = map(n.left, f)
    let newD = f(n.value)
    let newRight = map(n.right, f)
    Some({left: newLeft, key: n.key, value: newD, right: newRight, height: n.height})
  }

let rec mapWithKey = (n, f) =>
  switch n {
  | None => None
  | Some(n) =>
    let key = n.key
    let newLeft = mapWithKey(n.left, f)
    let newD = f(key, n.value)
    let newRight = mapWithKey(n.right, f)
    Some({left: newLeft, key, value: newD, right: newRight, height: n.height})
  }

let rec reduce = (m, accu, f) =>
  switch m {
  | None => accu
  | Some(n) =>
    let {left: l, key: v, value: d, right: r} = n
    reduce(r, f(reduce(l, accu, f), v, d), f)
  }

let rec every = (n, p) =>
  switch n {
  | None => true
  | Some(n) => p(n.key, n.value) && (every(n.left, p) && every(n.right, p))
  }

let rec some = (n, p) =>
  switch n {
  | None => false
  | Some(n) => p(n.key, n.value) || (some(n.left, p) || some(n.right, p))
  }
/* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*/

let rec addMinElement = (n, k, v) =>
  switch n {
  | None => singleton(k, v)
  | Some(n) => bal(addMinElement(n.left, k, v), n.key, n.value, n.right)
  }

let rec addMaxElement = (n, k, v) =>
  switch n {
  | None => singleton(k, v)
  | Some(n) => bal(n.left, n.key, n.value, addMaxElement(n.right, k, v))
  }

/* Same as create and bal, but no assumptions are made on the
 relative heights of l and r. */

let rec join = (ln, v, d, rn) =>
  switch (ln, rn) {
  | (None, _) => addMinElement(rn, v, d)
  | (_, None) => addMaxElement(ln, v, d)
  | (Some(l), Some(r)) =>
    let {left: ll, key: lv, value: ld, right: lr, height: lh} = l
    let {left: rl, key: rv, value: rd, right: rr, height: rh} = r
    if lh > rh + 2 {
      bal(ll, lv, ld, join(lr, v, d, rn))
    } else if rh > lh + 2 {
      bal(join(ln, v, d, rl), rv, rd, rr)
    } else {
      create(ln, v, d, rn)
    }
  }

/* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. */

let concat = (t1, t2) =>
  switch (t1, t2) {
  | (None, _) => t2
  | (_, None) => t1
  | (_, Some(t2n)) =>
    let (kr, vr) = (ref(t2n.key), ref(t2n.value))
    let t2r = removeMinAuxWithRef(t2n, kr, vr)
    join(t1, kr.contents, vr.contents, t2r)
  }

let concatOrJoin = (t1, v, d, t2) =>
  switch d {
  | Some(d) => join(t1, v, d, t2)
  | None => concat(t1, t2)
  }

let rec keepShared = (n, p) =>
  switch n {
  | None => None
  | Some(n) =>
    /* call `p` in the expected left-to-right order */
    let {key: v, value: d} = n
    let newLeft = keepShared(n.left, p)
    let pvd = p(v, d)
    let newRight = keepShared(n.right, p)
    if pvd {
      join(newLeft, v, d, newRight)
    } else {
      concat(newLeft, newRight)
    }
  }

let rec keepMap = (n, p) =>
  switch n {
  | None => None
  | Some(n) =>
    /* call `p` in the expected left-to-right order */
    let {key: v, value: d} = n
    let newLeft = keepMap(n.left, p)
    let pvd = p(v, d)
    let newRight = keepMap(n.right, p)
    switch pvd {
    | None => concat(newLeft, newRight)
    | Some(d) => join(newLeft, v, d, newRight)
    }
  }

let rec partitionShared = (n, p) =>
  switch n {
  | None => (None, None)
  | Some(n) =>
    let {key, value} = n
    /* call `p` in the expected left-to-right order */
    let (lt, lf) = partitionShared(n.left, p)
    let pvd = p(key, value)
    let (rt, rf) = partitionShared(n.right, p)
    if pvd {
      (join(lt, key, value, rt), concat(lf, rf))
    } else {
      (concat(lt, rt), join(lf, key, value, rf))
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
  | Some(n) =>
    let {left: l, right: r, key: k, value: v} = n
    toListAux(l, list{(k, v), ...toListAux(r, accu)})
  }

let toList = s => toListAux(s, list{})

let rec checkInvariantInternal = (v: t<_>) =>
  switch v {
  | None => ()
  | Some(n) =>
    let (l, r) = (n.left, n.right)
    let diff = treeHeight(l) - treeHeight(r)
    assert(diff <= 2 && diff >= -2)
    checkInvariantInternal(l)
    checkInvariantInternal(r)
  }

let rec fillArrayKey = (n, i, arr) => {
  let {left: l, key: v, right: r} = n
  let next = switch l {
  | None => i
  | Some(l) => fillArrayKey(l, i, arr)
  }
  A.setUnsafe(arr, next, v)
  let rnext = next + 1
  switch r {
  | None => rnext
  | Some(r) => fillArrayKey(r, rnext, arr)
  }
}

let rec fillArrayValue = (n, i, arr) => {
  let (l, r) = (n.left, n.right)
  let next = switch l {
  | None => i
  | Some(l) => fillArrayValue(l, i, arr)
  }
  A.setUnsafe(arr, next, n.value)
  let rnext = next + 1
  switch r {
  | None => rnext
  | Some(r) => fillArrayValue(r, rnext, arr)
  }
}

let rec fillArray = (n, i, arr) => {
  let (l, v, r) = (n.left, n.key, n.right)
  let next = switch l {
  | None => i
  | Some(l) => fillArray(l, i, arr)
  }
  A.setUnsafe(arr, next, (v, n.value))
  let rnext = next + 1
  switch r {
  | None => rnext
  | Some(r) => fillArray(r, rnext, arr)
  }
}

/* let rec fillArrayWithPartition n cursor arr p =
  let l,v,r = n.left, n.key , n.right in
  (match  l with
   | None -> ()
   | Some l ->
     fillArrayWithPartition l cursor arr p);
  (if p v [@bs] then begin
      let c = forwardGet cursor in
      A.setUnsafe arr c (v,n.value);
      forwardSet cursor (c + 1)
    end
   else begin
     let c = backwardGet cursor in
     A.setUnsafe arr c (v, n.value);
     backwardSet cursor (c - 1)
   end);
  match  r with
  | None -> ()
  | Some r ->
    fillArrayWithPartition r cursor arr  p

let rec fillArrayWithFilter n i arr p =
  let l,v,r = n.left, n.key , n.right in
  let next =
    match  l with
    | None -> i
    | Some l ->
      fillArrayWithFilter l i arr p in
  let rnext =
    if p v [@bs] then
      (A.setUnsafe arr next (v, n.value);
       next + 1
      )
    else next in
  match  r with
  | None -> rnext
  | Some r ->
    fillArrayWithFilter r rnext arr  p
 */

let toArray = n =>
  switch n {
  | None => []
  | Some(n) =>
    let size = lengthNode(n)
    let v = A.makeUninitializedUnsafe(size)
    ignore((fillArray(n, 0, v): int)) /* may add assertion */
    v
  }

let keysToArray = n =>
  switch n {
  | None => []
  | Some(n) =>
    let size = lengthNode(n)
    let v = A.makeUninitializedUnsafe(size)
    ignore((fillArrayKey(n, 0, v): int)) /* may add assertion */
    v
  }

let valuesToArray = n =>
  switch n {
  | None => []
  | Some(n) =>
    let size = lengthNode(n)
    let v = A.makeUninitializedUnsafe(size)
    ignore((fillArrayValue(n, 0, v): int)) /* may add assertion */
    v
  }

let rec fromSortedArrayRevAux = (arr, off, len) =>
  switch len {
  | 0 => None
  | 1 =>
    let (k, v) = A.getUnsafe(arr, off)
    singleton(k, v)
  | 2 =>
    let ((x0, y0), (x1, y1)) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off - 1))
    }

    Some({left: singleton(x0, y0), key: x1, value: y1, height: 2, right: None})
  | 3 =>
    let ((x0, y0), (x1, y1), (x2, y2)) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off - 1), getUnsafe(arr, off - 2))
    }
    Some({
      left: singleton(x0, y0),
      right: singleton(x2, y2),
      key: x1,
      value: y1,
      height: 2,
    })
  | _ =>
    let nl = len / 2
    let left = fromSortedArrayRevAux(arr, off, nl)
    let (midK, midV) = A.getUnsafe(arr, off - nl)
    let right = fromSortedArrayRevAux(arr, off - nl - 1, len - nl - 1)
    create(left, midK, midV, right)
  }

let rec fromSortedArrayAux = (arr, off, len) =>
  switch len {
  | 0 => None
  | 1 =>
    let (k, v) = A.getUnsafe(arr, off)
    singleton(k, v)
  | 2 =>
    let ((x0, y0), (x1, y1)) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off + 1))
    }

    Some({left: singleton(x0, y0), key: x1, value: y1, height: 2, right: None})
  | 3 =>
    let ((x0, y0), (x1, y1), (x2, y2)) = {
      open A
      (getUnsafe(arr, off), getUnsafe(arr, off + 1), getUnsafe(arr, off + 2))
    }
    Some({
      left: singleton(x0, y0),
      right: singleton(x2, y2),
      key: x1,
      value: y1,
      height: 2,
    })
  | _ =>
    let nl = len / 2
    let left = fromSortedArrayAux(arr, off, nl)
    let (midK, midV) = A.getUnsafe(arr, off + nl)
    let right = fromSortedArrayAux(arr, off + nl + 1, len - nl - 1)
    create(left, midK, midV, right)
  }

let fromSortedArrayUnsafe = arr => fromSortedArrayAux(arr, 0, A.length(arr))

let rec compareAux = (e1, e2, ~kcmp, ~vcmp) =>
  switch (e1, e2) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let c = Belt_Id.getCmpInternal(kcmp)(h1.key, h2.key)
    if c == 0 {
      let cx = vcmp(h1.value, h2.value)
      if cx == 0 {
        compareAux(~kcmp, ~vcmp, stackAllLeft(h1.right, t1), stackAllLeft(h2.right, t2))
      } else {
        cx
      }
    } else {
      c
    }
  | (_, _) => 0
  }

let rec eqAux = (e1, e2, ~kcmp, ~veq) =>
  switch (e1, e2) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    if Belt_Id.getCmpInternal(kcmp)(h1.key, h2.key) == 0 && veq(h1.value, h2.value) {
      eqAux(~kcmp, ~veq, stackAllLeft(h1.right, t1), stackAllLeft(h2.right, t2))
    } else {
      false
    }
  | (_, _) => true
  }

let cmp = (s1, s2, ~kcmp, ~vcmp) => {
  let (len1, len2) = (size(s1), size(s2))
  if len1 == len2 {
    compareAux(stackAllLeft(s1, list{}), stackAllLeft(s2, list{}), ~kcmp, ~vcmp)
  } else if len1 < len2 {
    -1
  } else {
    1
  }
}

let eq = (s1, s2, ~kcmp, ~veq) => {
  let (len1, len2) = (size(s1), size(s2))
  if len1 == len2 {
    eqAux(stackAllLeft(s1, list{}), stackAllLeft(s2, list{}), ~kcmp, ~veq)
  } else {
    false
  }
}

let rec get = (n, x, ~cmp) =>
  switch n {
  | None => None
  | Some(n) /* Node(l, v, d, r, _) */ =>
    let v = n.key
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      Some(n.value)
    } else {
      get(
        ~cmp,
        if c < 0 {
          n.left
        } else {
          n.right
        },
        x,
      )
    }
  }

let rec getUndefined = (n, x, ~cmp) =>
  switch n {
  | None => Js.undefined
  | Some(n) =>
    let v = n.key
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      Js.Undefined.return(n.value)
    } else {
      getUndefined(
        ~cmp,
        if c < 0 {
          n.left
        } else {
          n.right
        },
        x,
      )
    }
  }

let rec getOrThrow = (n, x, ~cmp) =>
  switch n {
  | None => throw(Not_found)
  | Some(n) /* Node(l, v, d, r, _) */ =>
    let v = n.key
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      n.value
    } else {
      getOrThrow(
        ~cmp,
        if c < 0 {
          n.left
        } else {
          n.right
        },
        x,
      )
    }
  }

let rec getWithDefault = (n, x, def, ~cmp) =>
  switch n {
  | None => def
  | Some(n) /* Node(l, v, d, r, _) */ =>
    let v = n.key
    let c = Belt_Id.getCmpInternal(cmp)(x, v)
    if c == 0 {
      n.value
    } else {
      getWithDefault(
        ~cmp,
        if c < 0 {
          n.left
        } else {
          n.right
        },
        x,
        def,
      )
    }
  }

let rec has = (n, x, ~cmp) =>
  switch n {
  | None => false
  | Some(n) /* Node(l, v, d, r, _) */ =>
    let v = n.key
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
    let (hlk2, hrk2) = (treeHeight(k2.left), treeHeight(k2.right))
    k2.height = Pervasives.max(hlk2, hrk2) + 1
    let (hlk1, hk2) = (treeHeight(k1.left), k2.height)
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
    let (hlk1, hrk1) = (treeHeight(k1.left), treeHeight(k1.right))
    k1.height = Pervasives.max(hlk1, hrk1) + 1
    let (hrk2, hk1) = (treeHeight(k2.right), k1.height)
    k2.height = Pervasives.max(hrk2, hk1) + 1
    k2
  }

/*
  double l rotation
*/
let doubleWithLeftChild = k3 => {
  let k3l = switch k3.left {
  | None => assert(false)
  | Some(x) => x
  }
  let v = rotateWithRightChild(k3l)
  k3.left = Some(v)
  rotateWithLeftChild(k3)
}

let doubleWithRightChild = k2 => {
  let k2r = switch k2.right {
  | None => assert(false)
  | Some(x) => x
  }
  let v = rotateWithLeftChild(k2r)
  k2.right = Some(v)
  rotateWithRightChild(k2)
}

let heightUpdateMutate = t => {
  let (hlt, hrt) = (treeHeight(t.left), treeHeight(t.right))
  t.height = Pervasives.max(hlt, hrt) + 1
  t
}

let balMutate = nt => {
  let (l, r) = (nt.left, nt.right)
  let (hl, hr) = (treeHeight(l), treeHeight(r))
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

let rec updateMutate = (t: t<_>, x, data, ~cmp) =>
  switch t {
  | None => singleton(x, data)
  | Some(nt) =>
    let k = nt.key
    let c = Belt_Id.getCmpInternal(cmp)(x, k)
    if c == 0 {
      nt.value = data
      Some(nt)
    } else {
      let (l, r) = (nt.left, nt.right)
      if c < 0 {
        let ll = updateMutate(~cmp, l, x, data)
        nt.left = ll
      } else {
        nt.right = updateMutate(~cmp, r, x, data)
      }
      Some(balMutate(nt))
    }
  }

let fromArray = (xs: array<_>, ~cmp) => {
  let len = A.length(xs)
  if len == 0 {
    None
  } else {
    let next = ref(
      S.strictlySortedLength(xs, ((x0, _), (y0, _)) => Belt_Id.getCmpInternal(cmp)(x0, y0) < 0),
    )

    let result = ref(
      if next.contents >= 0 {
        fromSortedArrayAux(xs, 0, next.contents)
      } else {
        next.contents = -next.contents
        fromSortedArrayRevAux(xs, next.contents - 1, next.contents)
      },
    )
    for i in next.contents to len - 1 {
      let (k, v) = A.getUnsafe(xs, i)
      result.contents = updateMutate(~cmp, result.contents, k, v)
    }
    result.contents
  }
}

let rec removeMinAuxWithRootMutate = (nt, n) => {
  let (rn, ln) = (n.right, n.left)
  switch ln {
  | None =>
    nt.key = n.key
    nt.value = n.value
    rn
  | Some(ln) =>
    n.left = removeMinAuxWithRootMutate(nt, ln)
    Some(balMutate(n))
  }
}
