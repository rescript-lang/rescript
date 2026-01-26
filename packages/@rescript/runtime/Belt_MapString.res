type key = string
module I = Belt_internalMapString

module N = Belt_internalAVLtree
module A = Belt_Array

type t<'a> = N.t<key, 'a>

let empty = None
let isEmpty = N.isEmpty
/* let singleton = N.singleton */

let minKey = N.minKey
let minKeyUndefined = N.minKeyUndefined
let maxKey = N.maxKey
let maxKeyUndefined = N.maxKeyUndefined
let minimum = N.minimum
let minUndefined = N.minUndefined
let maximum = N.maximum
let maxUndefined = N.maxUndefined
let findFirstBy = N.findFirstBy
let forEach = N.forEach
let map = N.map
let mapWithKey = N.mapWithKey
let reduce = N.reduce
let every = N.every
let some = N.some
let keep = N.keepShared
let partition = N.partitionShared
let size = N.size
let toList = N.toList
let toArray = N.toArray
let keysToArray = N.keysToArray
let valuesToArray = N.valuesToArray
let checkInvariantInternal = N.checkInvariantInternal

let rec set = (t, newK: key, newD: _) =>
  switch t {
  | None => N.singleton(newK, newD)
  | Some(n) =>
    let k = n.N.key
    if newK == k {
      Some(N.updateValue(n, newD))
    } else {
      let v = n.N.value
      if newK < k {
        N.bal(set(n.N.left, newK, newD), k, v, n.N.right)
      } else {
        N.bal(n.N.left, k, v, set(n.N.right, newK, newD))
      }
    }
  }

let rec update = (t, x: key, f) =>
  switch t {
  | None =>
    switch f(None) {
    | None => t
    | Some(data) => N.singleton(x, data)
    }
  | Some(n) =>
    let k = n.N.key
    if x == k {
      switch f(Some(n.N.value)) {
      | None =>
        let {N.left: l, right: r} = n
        switch (l, r) {
        | (None, _) => r
        | (_, None) => l
        | (_, Some(rn)) =>
          let (kr, vr) = (ref(rn.N.key), ref(rn.N.value))
          let r = N.removeMinAuxWithRef(rn, kr, vr)
          N.bal(l, kr.contents, vr.contents, r)
        }
      | Some(data) => Some(N.updateValue(n, data))
      }
    } else {
      let {N.left: l, right: r, value: v} = n
      if x < k {
        let ll = update(l, x, f)
        if l === ll {
          t
        } else {
          N.bal(ll, k, v, r)
        }
      } else {
        let rr = update(r, x, f)
        if r === rr {
          t
        } else {
          N.bal(l, k, v, rr)
        }
      }
    }
  }

let rec removeAux = (n, x: key) => {
  let {N.left: l, key: v, right: r} = n
  if x == v {
    switch (l, r) {
    | (None, _) => r
    | (_, None) => l
    | (_, Some(rn)) =>
      let (kr, vr) = (ref(rn.N.key), ref(rn.N.value))
      let r = N.removeMinAuxWithRef(rn, kr, vr)
      N.bal(l, kr.contents, vr.contents, r)
    }
  } else if x < v {
    switch l {
    | None => Some(n)
    | Some(left) =>
      let ll = removeAux(left, x)
      if ll === l {
        Some(n)
      } else {
        open N
        bal(ll, v, n.value, r)
      }
    }
  } else {
    switch r {
    | None => Some(n)
    | Some(right) =>
      let rr = removeAux(right, x)
      N.bal(l, v, n.N.value, rr)
    }
  }
}

let remove = (n, x) =>
  switch n {
  | None => None
  | Some(n) => removeAux(n, x)
  }

let rec removeMany0 = (t, xs, i, len) =>
  if i < len {
    let ele = A.getUnsafe(xs, i)
    let u = removeAux(t, ele)
    switch u {
    | None => u
    | Some(t) => removeMany0(t, xs, i + 1, len)
    }
  } else {
    Some(t)
  }

let removeMany = (t, keys) => {
  let len = A.length(keys)
  switch t {
  | None => None
  | Some(t) => removeMany0(t, keys, 0, len)
  }
}

let mergeMany = (h, arr) => {
  let len = A.length(arr)
  let v = ref(h)
  for i in 0 to len - 1 {
    let (key, value) = A.getUnsafe(arr, i)
    v.contents = set(v.contents, key, value)
  }
  v.contents
}

/* let mergeArray = mergeMany */

let has = I.has
let cmp = I.cmp
let eq = I.eq
let get = I.get
let getUndefined = I.getUndefined
let getWithDefault = I.getWithDefault
let getOrThrow = I.getOrThrow
let getExn = getOrThrow
let split = I.split
let merge = I.merge
let fromArray = I.fromArray

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
