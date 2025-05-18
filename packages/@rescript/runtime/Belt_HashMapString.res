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

/* Adapted by Hongbo Zhang, Authors of ReScript 2017 */

type key = string
type seed = int
external hash_mix_string: (seed, string) => seed = "%hash_mix_string"
external hash_final_mix: seed => seed = "%hash_final_mix"
let hash = (s: key) => hash_final_mix(hash_mix_string(0, s))

module N = Belt_internalBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type t<'b> = N.t<unit, unit, key, 'b>

let rec copyBucketReHash = (~h_buckets, ~ndata_tail, old_bucket: C.opt<N.bucket<_>>) =>
  switch C.toOpt(old_bucket) {
  | None => ()
  | Some(cell) =>
    let nidx = land(hash(cell.key), A.length(h_buckets) - 1)
    let v = C.return(cell)
    switch C.toOpt(A.getUnsafe(ndata_tail, nidx)) {
    | None => A.setUnsafe(h_buckets, nidx, v)
    | Some(tail) => tail.N.next = v /* cell put at the end */
    }
    A.setUnsafe(ndata_tail, nidx, v)
    copyBucketReHash(~h_buckets, ~ndata_tail, cell.next)
  }

let resize = h => {
  let odata = h.C.buckets
  let osize = A.length(odata)
  let nsize = osize * 2
  if nsize >= osize {
    /* no overflow */
    let h_buckets = A.makeUninitialized(nsize)
    let ndata_tail = A.makeUninitialized(nsize) /* keep track of tail */
    h.C.buckets = h_buckets /* so that indexfun sees the new bucket count */
    for i in 0 to osize - 1 {
      copyBucketReHash(~h_buckets, ~ndata_tail, A.getUnsafe(odata, i))
    }
    for i in 0 to nsize - 1 {
      switch C.toOpt(A.getUnsafe(ndata_tail, i)) {
      | None => ()
      | Some(tail) => tail.next = C.emptyOpt
      }
    }
  }
}

let rec replaceInBucket = (key: key, info, cell) =>
  if cell.N.key == key {
    cell.N.value = info
    false
  } else {
    switch C.toOpt(cell.next) {
    | None => true
    | Some(cell) => replaceInBucket(key, info, cell)
    }
  }

let set = (h, key: key, value) => {
  let h_buckets = h.C.buckets
  let buckets_len = A.length(h_buckets)
  let i = land(hash(key), buckets_len - 1)
  let l = A.getUnsafe(h_buckets, i)
  switch C.toOpt(l) {
  | None =>
    A.setUnsafe(h_buckets, i, C.return({N.key, value, next: C.emptyOpt}))
    h.C.size = h.C.size + 1
  | Some(bucket) =>
    if replaceInBucket(key, value, bucket) {
      A.setUnsafe(h_buckets, i, C.return({N.key, value, next: l}))
      h.C.size = h.C.size + 1
    }
  }
  if h.C.size > lsl(buckets_len, 1) {
    resize(h)
  }
}

let rec removeInBucket = (h, h_buckets, i, key: key, prec, buckets) =>
  switch C.toOpt(buckets) {
  | None => ()
  | Some(cell) =>
    let cell_next = cell.N.next
    if cell.N.key == key {
      prec.N.next = cell_next
      h.C.size = h.C.size - 1
    } else {
      removeInBucket(h, h_buckets, i, key, cell, cell_next)
    }
  }

let remove = (h, key) => {
  let h_buckets = h.C.buckets
  let i = land(hash(key), A.length(h_buckets) - 1)
  let bucket = A.getUnsafe(h_buckets, i)
  switch C.toOpt(bucket) {
  | None => ()
  | Some(cell) =>
    if cell.N.key == key {
      A.setUnsafe(h_buckets, i, cell.next)
      h.C.size = h.C.size - 1
    } else {
      removeInBucket(h, h_buckets, i, key, cell, cell.next)
    }
  }
}

let rec getAux = (key: key, buckets) =>
  switch C.toOpt(buckets) {
  | None => None
  | Some(cell) =>
    if key == cell.N.key {
      Some(cell.N.value)
    } else {
      getAux(key, cell.next)
    }
  }

let get = (h, key: key) => {
  let h_buckets = h.C.buckets
  let nid = land(hash(key), A.length(h_buckets) - 1)
  switch C.toOpt(A.getUnsafe(h_buckets, nid)) {
  | None => None
  | Some(cell1) =>
    if key == cell1.N.key {
      Some(cell1.N.value)
    } else {
      switch C.toOpt(cell1.N.next) {
      | None => None
      | Some(cell2) =>
        if key == cell2.N.key {
          Some(cell2.N.value)
        } else {
          switch C.toOpt(cell2.N.next) {
          | None => None
          | Some(cell3) =>
            if key == cell3.N.key {
              Some(cell3.N.value)
            } else {
              getAux(key, cell3.N.next)
            }
          }
        }
      }
    }
  }
}

let rec memInBucket = (key: key, cell) =>
  cell.N.key == key ||
    switch C.toOpt(cell.next) {
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
let keepMapInPlace = N.keepMapInPlace
let toArray = N.toArray
let copy = N.copy
let keysToArray = N.keysToArray
let valuesToArray = N.valuesToArray
let getBucketHistogram = N.getBucketHistogram
let isEmpty = C.isEmpty

let fromArray = arr => {
  let len = A.length(arr)
  let v = make(~hintSize=len)
  for i in 0 to len - 1 {
    let (k, value) = A.getUnsafe(arr, i)
    set(v, k, value)
  }
  v
}

/* TOOD: optimize heuristics for resizing */
let mergeMany = (h, arr) => {
  let len = A.length(arr)
  for i in 0 to len - 1 {
    let (k, v) = A.getUnsafe(arr, i)
    set(h, k, v)
  }
}

let forEachU = forEach
let reduceU = reduce
let keepMapInPlaceU = keepMapInPlace
