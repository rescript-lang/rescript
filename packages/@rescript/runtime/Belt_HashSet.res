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

module Int = Belt_HashSetInt

module String = Belt_HashSetString

module N = Belt_internalSetBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type eq<'a, 'id> = Belt_Id.eq<'a, 'id>
type hash<'a, 'id> = Belt_Id.hash<'a, 'id>
type id<'a, 'id> = Belt_Id.hashable<'a, 'id>

type t<'a, 'id> = N.t<hash<'a, 'id>, eq<'a, 'id>, 'a>

let rec copyBucket = (~hash, ~h_buckets, ~ndata_tail, old_bucket) =>
  switch C.toOpt(old_bucket) {
  | None => ()
  | Some(cell) =>
    let nidx = land(Belt_Id.getHashInternal(hash)(cell.N.key), A.length(h_buckets) - 1)
    let v = C.return(cell)
    switch C.toOpt(A.getUnsafe(ndata_tail, nidx)) {
    | None => A.setUnsafe(h_buckets, nidx, v)
    | Some(tail) => tail.N.next = v /* cell put at the end */
    }
    A.setUnsafe(ndata_tail, nidx, v)
    copyBucket(~hash, ~h_buckets, ~ndata_tail, cell.N.next)
  }

let tryDoubleResize = (~hash, h) => {
  let odata = h.C.buckets
  let osize = A.length(odata)
  let nsize = osize * 2
  if nsize >= osize {
    /* no overflow */
    let h_buckets = A.makeUninitialized(nsize)
    let ndata_tail = A.makeUninitialized(nsize) /* keep track of tail */
    h.buckets = h_buckets /* so that indexfun sees the new bucket count */
    for i in 0 to osize - 1 {
      copyBucket(~hash, ~h_buckets, ~ndata_tail, A.getUnsafe(odata, i))
    }
    for i in 0 to nsize - 1 {
      switch C.toOpt(A.getUnsafe(ndata_tail, i)) {
      | None => ()
      | Some(tail) => tail.N.next = C.emptyOpt
      }
    }
  }
}

let rec removeBucket = (~eq, h, h_buckets, i, key, prec, cell) => {
  let cell_next = cell.N.next
  if Belt_Id.getEqInternal(eq)(cell.N.key, key) {
    prec.N.next = cell_next
    h.C.size = h.C.size - 1
  } else {
    switch C.toOpt(cell_next) {
    | None => ()
    | Some(cell_next) => removeBucket(~eq, h, h_buckets, i, key, cell, cell_next)
    }
  }
}

let remove = (h, key) => {
  let eq = h.C.eq
  let h_buckets = h.C.buckets
  let i = land(Belt_Id.getHashInternal(h.C.hash)(key), A.length(h_buckets) - 1)
  let l = A.getUnsafe(h_buckets, i)
  switch C.toOpt(l) {
  | None => ()
  | Some(cell) =>
    let next_cell = cell.N.next
    if Belt_Id.getEqInternal(eq)(cell.N.key, key) {
      h.C.size = h.C.size - 1
      A.setUnsafe(h_buckets, i, next_cell)
    } else {
      switch C.toOpt(next_cell) {
      | None => ()
      | Some(next_cell) => removeBucket(~eq, h, h_buckets, i, key, cell, next_cell)
      }
    }
  }
}

let rec addBucket = (h, key, cell, ~eq) =>
  if !Belt_Id.getEqInternal(eq)(cell.N.key, key) {
    let n = cell.N.next
    switch C.toOpt(n) {
    | None =>
      h.C.size = h.C.size + 1
      cell.N.next = C.return({N.key, next: C.emptyOpt})
    | Some(n) => addBucket(~eq, h, key, n)
    }
  }

let add0 = (h, key, ~hash, ~eq) => {
  let h_buckets = h.C.buckets
  let buckets_len = A.length(h_buckets)
  let i = land(Belt_Id.getHashInternal(hash)(key), buckets_len - 1)
  let l = A.getUnsafe(h_buckets, i)
  switch C.toOpt(l) {
  | None =>
    h.C.size = h.C.size + 1
    A.setUnsafe(h_buckets, i, C.return({N.key, next: C.emptyOpt}))
  | Some(cell) => addBucket(~eq, h, key, cell)
  }
  if h.C.size > lsl(buckets_len, 1) {
    tryDoubleResize(~hash, h)
  }
}

let add = (h, key) => add0(~hash=h.C.hash, ~eq=h.C.eq, h, key)

let rec memInBucket = (~eq, key, cell) =>
  Belt_Id.getEqInternal(eq)(cell.N.key, key) ||
  switch C.toOpt(cell.N.next) {
  | None => false
  | Some(nextCell) => memInBucket(~eq, key, nextCell)
  }

let has = (h, key) => {
  let (eq, h_buckets) = (h.C.eq, h.C.buckets)
  let nid = land(Belt_Id.getHashInternal(h.C.hash)(key), A.length(h_buckets) - 1)
  let bucket = A.getUnsafe(h_buckets, nid)
  switch C.toOpt(bucket) {
  | None => false
  | Some(bucket) => memInBucket(~eq, key, bucket)
  }
}

let make = (type value identity, ~hintSize, ~id: id<value, identity>) => {
  module M = unpack(id)
  C.make(~hintSize, ~hash=M.hash, ~eq=M.eq)
}

let clear = C.clear
let size = h => h.C.size
let forEach = N.forEach
let reduce = N.reduce
let logStats = N.logStats
let toArray = N.toArray
let copy = N.copy
let getBucketHistogram = N.getBucketHistogram
let isEmpty = C.isEmpty

let fromArray = (type a identity, arr, ~id: id<a, identity>) => {
  module M = unpack(id)
  let (eq, hash) = (M.eq, M.hash)
  let len = A.length(arr)
  let v = C.make(~hintSize=len, ~hash, ~eq)
  for i in 0 to len - 1 {
    add0(~eq, ~hash, v, A.getUnsafe(arr, i))
  }
  v
}

let mergeMany = (h, arr) => {
  let (eq, hash) = (h.C.eq, h.hash)
  let len = A.length(arr)
  for i in 0 to len - 1 {
    add0(h, ~eq, ~hash, A.getUnsafe(arr, i))
  }
}

let forEachU = forEach
let reduceU = reduce
