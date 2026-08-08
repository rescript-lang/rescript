type key = string
type seed = int
external hash_mix_string: (seed, string) => seed = "%hash_mix_string"
external hash_final_mix: seed => seed = "%hash_final_mix"
let hash = (s: key) => hash_final_mix(hash_mix_string(0, s))

module N = Belt_internalSetBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type t = N.t<unit, unit, key>

let rec copyBucket = (~h_buckets, ~ndata_tail, old_bucket) =>
  switch C.toOpt(old_bucket) {
  | None => ()
  | Some(cell) =>
    let nidx = land(hash(cell.N.key), A.length(h_buckets) - 1)
    let v = C.return(cell)
    switch C.toOpt(A.getUnsafe(ndata_tail, nidx)) {
    | None => A.setUnsafe(h_buckets, nidx, v)
    | Some(tail) => tail.N.next = v /* cell put at the end */
    }
    A.setUnsafe(ndata_tail, nidx, v)
    copyBucket(~h_buckets, ~ndata_tail, cell.N.next)
  }

let tryDoubleResize = h => {
  let odata = h.C.buckets
  let osize = A.length(odata)
  let nsize = osize * 2
  if nsize >= osize {
    /* no overflow */
    let h_buckets = A.makeUninitialized(nsize)
    let ndata_tail = A.makeUninitialized(nsize) /* keep track of tail */
    h.C.buckets = h_buckets /* so that indexfun sees the new bucket count */
    for i in 0 to osize - 1 {
      copyBucket(~h_buckets, ~ndata_tail, A.getUnsafe(odata, i))
    }
    for i in 0 to nsize - 1 {
      switch C.toOpt(A.getUnsafe(ndata_tail, i)) {
      | None => ()
      | Some(tail) => tail.N.next = C.emptyOpt
      }
    }
  }
}

let rec removeBucket = (h, h_buckets, i, key: key, prec, cell) => {
  let cell_next = cell.N.next
  if cell.N.key == key {
    prec.N.next = cell_next
    h.C.size = h.C.size - 1
  } else {
    switch C.toOpt(cell_next) {
    | None => ()
    | Some(cell_next) => removeBucket(h, h_buckets, i, key, cell, cell_next)
    }
  }
}

let remove = (h, key: key) => {
  let h_buckets = h.C.buckets
  let i = land(hash(key), A.length(h_buckets) - 1)
  let l = A.getUnsafe(h_buckets, i)
  switch C.toOpt(l) {
  | None => ()
  | Some(cell) =>
    let next_cell = cell.N.next
    if cell.N.key == key {
      h.C.size = h.C.size - 1
      A.setUnsafe(h_buckets, i, next_cell)
    } else {
      switch C.toOpt(next_cell) {
      | None => ()
      | Some(next_cell) => removeBucket(h, h_buckets, i, key, cell, next_cell)
      }
    }
  }
}

let rec addBucket = (h, key: key, cell) =>
  if cell.N.key != key {
    let n = cell.N.next
    switch C.toOpt(n) {
    | None =>
      h.C.size = h.C.size + 1
      cell.N.next = C.return({N.key, next: C.emptyOpt})
    | Some(n) => addBucket(h, key, n)
    }
  }

let add = (h, key: key) => {
  let h_buckets = h.C.buckets
  let buckets_len = A.length(h_buckets)
  let i = land(hash(key), buckets_len - 1)
  let l = A.getUnsafe(h_buckets, i)
  switch C.toOpt(l) {
  | None =>
    A.setUnsafe(h_buckets, i, C.return({N.key, next: C.emptyOpt}))
    h.C.size = h.C.size + 1
  | Some(cell) => addBucket(h, key, cell)
  }
  if h.C.size > lsl(buckets_len, 1) {
    tryDoubleResize(h)
  }
}

let rec memInBucket = (key: key, cell) =>
  cell.N.key == key ||
    switch C.toOpt(cell.N.next) {
    | None => false
    | Some(nextCell) => memInBucket(key, nextCell)
    }

let has = (h, key) => {
  let h_buckets = h.C.buckets
  let nid = land(hash(key), A.length(h_buckets) - 1)
  let bucket = A.getUnsafe(h_buckets, nid)
  switch C.toOpt(bucket) {
  | None => false
  | Some(bucket) => memInBucket(key, bucket)
  }
}

let make = (~hintSize) => C.make(~hintSize, ~hash=(), ~eq=())

let clear = C.clear
let size = h => h.C.size
let forEach = N.forEach
let reduce = N.reduce
let logStats = N.logStats
let toArray = N.toArray
let copy = N.copy
let getBucketHistogram = N.getBucketHistogram
let isEmpty = C.isEmpty

let fromArray = arr => {
  let len = A.length(arr)
  let v = C.make(~hintSize=len, ~hash=(), ~eq=())
  for i in 0 to len - 1 {
    add(v, A.getUnsafe(arr, i))
  }
  v
}

/* TOOD: optimize heuristics for resizing */
let mergeMany = (h, arr) => {
  let len = A.length(arr)
  for i in 0 to len - 1 {
    add(h, A.getUnsafe(arr, i))
  }
}

let forEachU = forEach
let reduceU = reduce
