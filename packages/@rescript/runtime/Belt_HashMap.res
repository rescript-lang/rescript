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
/* Adapted by Hongbo Zhang, Authors of ReScript 2017 */
/* ********************************************************************* */

module N = Belt_internalBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type eq<'a, 'id> = Belt_Id.eq<'a, 'id>
type hash<'a, 'id> = Belt_Id.hash<'a, 'id>
type id<'a, 'id> = Belt_Id.hashable<'a, 'id>
type t<'a, 'b, 'id> = N.t<hash<'a, 'id>, eq<'a, 'id>, 'a, 'b>

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

let rec copyBucketReHash = (~hash, ~h_buckets, ~ndata_tail, old_bucket) =>
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
    copyBucketReHash(~hash, ~h_buckets, ~ndata_tail, cell.N.next)
  }

let resize = (~hash, h) => {
  let odata = h.C.buckets
  let osize = A.length(odata)
  let nsize = osize * 2
  if nsize >= osize {
    /* no overflow */
    let h_buckets = A.makeUninitialized(nsize)
    let ndata_tail = A.makeUninitialized(nsize) /* keep track of tail */
    h.C.buckets = h_buckets /* so that indexfun sees the new bucket count */
    for i in 0 to osize - 1 {
      copyBucketReHash(~hash, ~h_buckets, ~ndata_tail, A.getUnsafe(odata, i))
    }
    for i in 0 to nsize - 1 {
      switch C.toOpt(A.getUnsafe(ndata_tail, i)) {
      | None => ()
      | Some(tail) => tail.N.next = C.emptyOpt
      }
    }
  }
}

let rec replaceInBucket = (~eq, key, info, cell) =>
  if eq(cell.N.key, key) {
    cell.N.value = info
    false
  } else {
    switch C.toOpt(cell.N.next) {
    | None => true
    | Some(cell) => replaceInBucket(~eq, key, info, cell)
    }
  }

let set0 = (h, key, value, ~eq, ~hash) => {
  let h_buckets = h.C.buckets
  let buckets_len = A.length(h_buckets)
  let i = land(hash(key), buckets_len - 1)
  let l = A.getUnsafe(h_buckets, i)
  switch C.toOpt(l) {
  | None =>
    A.setUnsafe(h_buckets, i, C.return({N.key, value, next: C.emptyOpt}))
    h.C.size = h.C.size + 1
  | Some(bucket) =>
    if replaceInBucket(~eq, key, value, bucket) {
      A.setUnsafe(h_buckets, i, C.return({N.key, value, next: l}))
      h.C.size = h.C.size + 1
    }
  }
  if h.C.size > lsl(buckets_len, 1) {
    resize(~hash, h)
  }
}

/* if `key` already exists, replace it, otherwise add it 
   Here we add it to the head, it could be tail
*/
let set = (h, key, value) =>
  set0(h, key, value, ~eq=Belt_Id.getEqInternal(h.C.eq), ~hash=Belt_Id.getHashInternal(h.C.hash))

let rec removeInBucket = (h, h_buckets, i, key, prec, bucket, ~eq) =>
  switch C.toOpt(bucket) {
  | None => ()
  | Some(cell) =>
    let cell_next = cell.N.next
    if eq(cell.N.key, key) {
      prec.N.next = cell_next
      h.C.size = h.C.size - 1
    } else {
      removeInBucket(~eq, h, h_buckets, i, key, cell, cell_next)
    }
  }

let remove = (h, key) => {
  let h_buckets = h.C.buckets
  let i = land(Belt_Id.getHashInternal(h.C.hash)(key), A.length(h_buckets) - 1)
  let bucket = A.getUnsafe(h_buckets, i)
  switch C.toOpt(bucket) {
  | None => ()
  | Some(cell) =>
    let eq = Belt_Id.getEqInternal(h.C.eq)
    if eq(cell.N.key, key) {
      A.setUnsafe(h_buckets, i, cell.N.next)
      h.C.size = h.C.size - 1
    } else {
      removeInBucket(~eq, h, h_buckets, i, key, cell, cell.N.next)
    }
  }
}

let rec getAux = (~eq, key, buckets) =>
  switch C.toOpt(buckets) {
  | None => None
  | Some(cell) =>
    if eq(key, cell.N.key) {
      Some(cell.N.value)
    } else {
      getAux(~eq, key, cell.N.next)
    }
  }

let get = (h, key) => {
  let h_buckets = h.C.buckets
  let nid = land(Belt_Id.getHashInternal(h.C.hash)(key), A.length(h_buckets) - 1)
  switch C.toOpt(A.getUnsafe(h_buckets, nid)) {
  | None => None
  | Some(cell1: N.bucket<_>) =>
    let eq = Belt_Id.getEqInternal(h.C.eq)
    if eq(key, cell1.key) {
      Some(cell1.value)
    } else {
      switch C.toOpt(cell1.N.next) {
      | None => None
      | Some(cell2) =>
        if eq(key, cell2.key) {
          Some(cell2.value)
        } else {
          switch C.toOpt(cell2.next) {
          | None => None
          | Some(cell3) =>
            if eq(key, cell3.key) {
              Some(cell3.value)
            } else {
              getAux(~eq, key, cell3.next)
            }
          }
        }
      }
    }
  }
}

let rec memInBucket = (key, cell, ~eq) =>
  eq(cell.N.key, key) ||
  switch C.toOpt(cell.N.next) {
  | None => false
  | Some(nextCell) => memInBucket(~eq, key, nextCell)
  }

let has = (h, key) => {
  let h_buckets = h.C.buckets
  let nid = land(Belt_Id.getHashInternal(h.C.hash)(key), A.length(h_buckets) - 1)
  let bucket = A.getUnsafe(h_buckets, nid)
  switch C.toOpt(bucket) {
  | None => false
  | Some(bucket) => memInBucket(~eq=Belt_Id.getEqInternal(h.C.eq), key, bucket)
  }
}

let make = (type key identity, ~hintSize, ~id: id<key, identity>) => {
  module M = unpack(id)
  C.make(~hash=M.hash, ~eq=M.eq, ~hintSize)
}

let fromArray = (type a identity, arr, ~id: id<a, identity>) => {
  module M = unpack(id)
  let (hash, eq) = (M.hash, M.eq)
  let len = A.length(arr)
  let v = C.make(~hash, ~eq, ~hintSize=len)
  let (eq, hash) = (Belt_Id.getEqInternal(eq), Belt_Id.getHashInternal(hash))
  for i in 0 to len - 1 {
    let (key, value) = A.getUnsafe(arr, i)
    set0(~eq, ~hash, v, key, value)
  }
  v
}

let mergeMany = (h, arr) => {
  let (hash, eq) = (Belt_Id.getHashInternal(h.C.hash), Belt_Id.getEqInternal(h.C.eq))
  let len = A.length(arr)
  for i in 0 to len - 1 {
    let (key, value) = A.getUnsafe(arr, i)
    set0(h, ~eq, ~hash, key, value)
  }
}

module Int = Belt_HashMapInt
module String = Belt_HashMapString

let forEachU = forEach
let reduceU = reduce
let keepMapInPlaceU = keepMapInPlace
