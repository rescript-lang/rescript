module I = Belt_internalSetString

module N = Belt_internalAVLset
module A = Belt_Array

type value = I.value
type t = I.t

let empty = None
let isEmpty = N.isEmpty
let minimum = N.minimum
let minUndefined = N.minUndefined
let maximum = N.maximum
let maxUndefined = N.maxUndefined

let forEach = N.forEach
let reduce = N.reduce
let every = N.every
let some = N.some
let keep = N.keepShared
let partition = N.partitionShared

let size = N.size
let toList = N.toList
let toArray = N.toArray
let fromSortedArrayUnsafe = N.fromSortedArrayUnsafe
let checkInvariantInternal = N.checkInvariantInternal

let rec add = (t: t, x: value): t =>
  switch t {
  | None => N.singleton(x)
  | Some(nt) =>
    let v = nt.value
    if x == v {
      t
    } else {
      let {N.left: l, right: r} = nt
      if x < v {
        let ll = add(l, x)
        if ll === l {
          t
        } else {
          N.bal(ll, v, r)
        }
      } else {
        let rr = add(r, x)
        if rr === r {
          t
        } else {
          N.bal(l, v, rr)
        }
      }
    }
  }

let mergeMany = (h, arr) => {
  let len = A.length(arr)
  let v = ref(h)
  for i in 0 to len - 1 {
    let key = A.getUnsafe(arr, i)
    v.contents = add(v.contents, key)
  }
  v.contents
}

let rec remove = (t: t, x: value): t =>
  switch t {
  | None => t
  | Some(n) =>
    let {N.left: l, value: v, right: r} = n
    if x == v {
      switch (l, r) {
      | (None, _) => r
      | (_, None) => l
      | (_, Some(rn)) =>
        let v = ref(rn.value)
        let r = N.removeMinAuxWithRef(rn, v)
        N.bal(l, v.contents, r)
      }
    } else if x < v {
      let ll = remove(l, x)
      if ll === l {
        t
      } else {
        N.bal(ll, v, r)
      }
    } else {
      let rr = remove(r, x)
      if rr === r {
        t
      } else {
        N.bal(l, v, rr)
      }
    }
  }

let removeMany = (h, arr) => {
  let len = A.length(arr)
  let v = ref(h)
  for i in 0 to len - 1 {
    let key = A.getUnsafe(arr, i)
    v.contents = remove(v.contents, key)
  }
  v.contents
}

let fromArray = I.fromArray
let cmp = I.cmp
let eq = I.eq
let get = I.get
let getUndefined = I.getUndefined
let getOrThrow = I.getOrThrow
let getExn = getOrThrow
let subset = I.subset
let has = I.has

let rec splitAuxNoPivot = (n: N.node<_>, x: value): (t, t) => {
  let {N.left: l, value: v, right: r} = n
  if x == v {
    (l, r)
  } else if x < v {
    switch l {
    | None => (None, Some(n))
    | Some(l) =>
      let (ll, rl) = splitAuxNoPivot(l, x)
      (ll, N.joinShared(rl, v, r))
    }
  } else {
    switch r {
    | None => (Some(n), None)
    | Some(r) =>
      let (lr, rr) = splitAuxNoPivot(r, x)
      (N.joinShared(l, v, lr), rr)
    }
  }
}

let rec splitAuxPivot = (n: N.node<_>, x: value, pres): (t, t) => {
  let {N.left: l, value: v, right: r} = n
  if x == v {
    pres.contents = true
    (l, r)
  } else if x < v {
    switch l {
    | None => (None, Some(n))
    | Some(l) =>
      let (ll, rl) = splitAuxPivot(l, x, pres)
      (ll, N.joinShared(rl, v, r))
    }
  } else {
    switch r {
    | None => (Some(n), None)
    | Some(r) =>
      let (lr, rr) = splitAuxPivot(r, x, pres)
      (N.joinShared(l, v, lr), rr)
    }
  }
}

let split = (t: t, x: value) =>
  switch t {
  | None => ((None, None), false)
  | Some(n) =>
    let pres = ref(false)
    let v = splitAuxPivot(n, x, pres)
    (v, pres.contents)
  }

let rec union = (s1: t, s2: t) =>
  switch (s1, s2) {
  | (None, _) => s2
  | (_, None) => s1
  | (Some(n1), Some(n2)) /* (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) */ =>
    let (h1, h2) = (n1.height, n2.height)
    if h1 >= h2 {
      if h2 == 1 {
        add(s1, n2.value)
      } else {
        let {N.left: l1, value: v1, right: r1} = n1
        let (l2, r2) = splitAuxNoPivot(n2, v1)
        N.joinShared(union(l1, l2), v1, union(r1, r2))
      }
    } else if h1 == 1 {
      add(s2, n1.value)
    } else {
      let {N.left: l2, value: v2, right: r2} = n2
      let (l1, r1) = splitAuxNoPivot(n1, v2)
      N.joinShared(union(l1, l2), v2, union(r1, r2))
    }
  }

let rec intersect = (s1: t, s2: t) =>
  switch (s1, s2) {
  | (None, _) | (_, None) => None
  | (Some(n1), Some(n2)) /* (Node(l1, v1, r1, _), t2) */ =>
    let {N.left: l1, value: v1, right: r1} = n1
    let pres = ref(false)
    let (l2, r2) = splitAuxPivot(n2, v1, pres)
    let ll = intersect(l1, l2)
    let rr = intersect(r1, r2)
    if pres.contents {
      N.joinShared(ll, v1, rr)
    } else {
      N.concatShared(ll, rr)
    }
  }

let rec diff = (s1: t, s2: t) =>
  switch (s1, s2) {
  | (None, _) | (_, None) => s1
  | (Some(n1), Some(n2)) /* (Node(l1, v1, r1, _), t2) */ =>
    let {N.left: l1, value: v1, right: r1} = n1
    let pres = ref(false)
    let (l2, r2) = splitAuxPivot(n2, v1, pres)
    let ll = diff(l1, l2)
    let rr = diff(r1, r2)
    if pres.contents {
      N.concatShared(ll, rr)
    } else {
      N.joinShared(ll, v1, rr)
    }
  }

let forEachU = forEach
let reduceU = reduce
let everyU = every
let someU = some
let keepU = keep
let partitionU = partition
