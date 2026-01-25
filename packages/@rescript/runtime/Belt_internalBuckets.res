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

/* For JS backends, we use `undefined` as default value, so that buckets
   could be allocated lazily
*/

/* We do dynamic hashing, and resize the table and rehash the elements
 when buckets become too long. */
module C = Belt_internalBucketsType
/* TODO:
   the current implementation relies on the fact that bucket 
   empty value is `undefined` in both places,
   in theory, it can be different 

*/
type rec bucket<'a, 'b> = {
  mutable key: 'a,
  mutable value: 'b,
  mutable next: C.opt<bucket<'a, 'b>>,
}
and t<'hash, 'eq, 'a, 'b> = C.container<'hash, 'eq, bucket<'a, 'b>>

module A = Belt_Array

let rec copy = (x: t<_>): t<_> => {
  hash: x.hash,
  eq: x.eq,
  size: x.size,
  buckets: copyBuckets(x.buckets),
}
and copyBuckets = (buckets: array<C.opt<bucket<_>>>) => {
  let len = A.length(buckets)
  let newBuckets = A.makeUninitializedUnsafe(len)
  for i in 0 to len - 1 {
    A.setUnsafe(newBuckets, i, copyBucket(A.getUnsafe(buckets, i)))
  }
  newBuckets
}
and copyBucket = c =>
  switch C.toOpt(c) {
  | None => c
  | Some(c) =>
    let head = {
      key: c.key,
      value: c.value,
      next: C.emptyOpt,
    }
    copyAuxCont(c.next, head)
    C.return(head)
  }
and copyAuxCont = (c, prec) =>
  switch C.toOpt(c) {
  | None => ()
  | Some(nc) =>
    let ncopy = {key: nc.key, value: nc.value, next: C.emptyOpt}
    prec.next = C.return(ncopy)
    copyAuxCont(nc.next, ncopy)
  }

let rec bucketLength = (accu, buckets) =>
  switch C.toOpt(buckets) {
  | None => accu
  | Some(cell) => bucketLength(accu + 1, cell.next)
  }

let rec do_bucket_iter = (~f, buckets) =>
  switch C.toOpt(buckets) {
  | None => ()
  | Some(cell) =>
    f(cell.key, cell.value)->ignore
    do_bucket_iter(~f, cell.next)
  }

let forEach = (h, f) => {
  let d = h.C.buckets
  for i in 0 to A.length(d) - 1 {
    do_bucket_iter(~f, A.getUnsafe(d, i))
  }
}

let rec do_bucket_fold = (~f, b, accu) =>
  switch C.toOpt(b) {
  | None => accu
  | Some(cell) => do_bucket_fold(~f, cell.next, f(accu, cell.key, cell.value))
  }

let reduce = (h, init, f) => {
  let d = h.C.buckets
  let accu = ref(init)
  for i in 0 to A.length(d) - 1 {
    accu.contents = do_bucket_fold(~f, A.getUnsafe(d, i), accu.contents)
  }
  accu.contents
}

let getMaxBucketLength = h =>
  A.reduce(h.C.buckets, 0, (m, b) => {
    let len = bucketLength(0, b)
    Pervasives.max(m, len)
  })

let getBucketHistogram = h => {
  let mbl = getMaxBucketLength(h)
  let histo = A.makeBy(mbl + 1, _ => 0)
  A.forEach(h.C.buckets, b => {
    let l = bucketLength(0, b)
    A.setUnsafe(histo, l, A.getUnsafe(histo, l) + 1)
  })
  histo
}

let logStats = h => {
  let histogram = getBucketHistogram(h)
  Js.log({
    "bindings": h.C.size,
    "buckets": A.length(h.C.buckets),
    "histogram": histogram,
  })
}

/** iterate the Buckets, in place remove the elements */
let rec filterMapInplaceBucket = (f, h, i, prec, cell) => {
  let n = cell.next
  switch f(cell.key, cell.value) {
  | None =>
    h.C.size = h.C.size - 1 /* delete */
    switch C.toOpt(n) {
    | Some(nextCell) => filterMapInplaceBucket(f, h, i, prec, nextCell)
    | None =>
      switch C.toOpt(prec) {
      | None => A.setUnsafe(h.C.buckets, i, prec)
      | Some(cell) => cell.next = n
      }
    }
  | Some(data) =>
    /* replace */
    let bucket = C.return(cell)
    switch C.toOpt(prec) {
    | None => A.setUnsafe(h.C.buckets, i, bucket)
    | Some(_) => cell.next = bucket
    }
    cell.value = data
    switch C.toOpt(n) {
    | None => cell.next = n
    | Some(nextCell) => filterMapInplaceBucket(f, h, i, bucket, nextCell)
    }
  }
}

let keepMapInPlace = (h, f) => {
  let h_buckets = h.C.buckets
  for i in 0 to A.length(h_buckets) - 1 {
    let v = A.getUnsafe(h_buckets, i)
    switch C.toOpt(v) {
    | None => ()
    | Some(v) => filterMapInplaceBucket(f, h, i, C.emptyOpt, v)
    }
  }
}

let rec fillArray = (i, arr, cell) => {
  A.setUnsafe(arr, i, (cell.key, cell.value))
  switch C.toOpt(cell.next) {
  | None => i + 1
  | Some(v) => fillArray(i + 1, arr, v)
  }
}

/* let toArray h = 
  let d =h.bucketsin 
  let current = ref 0 in 
  let arr = A.makeUninitializedUnsafe (C.sizeGet h) in 
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current .contents<- fillArray current.contents arr cell
  done;
  arr */

let rec fillArrayMap = (i, arr, cell, f) => {
  A.setUnsafe(arr, i, f(cell))
  switch C.toOpt(cell.next) {
  | None => i + 1
  | Some(v) => fillArrayMap(i + 1, arr, v, f)
  }
}

let linear = (h, f) => {
  let d = h.C.buckets
  let current = ref(0)
  let arr = A.makeUninitializedUnsafe(h.C.size)
  for i in 0 to A.length(d) - 1 {
    let cell = A.getUnsafe(d, i)
    switch C.toOpt(cell) {
    | None => ()
    | Some(cell) => current.contents = fillArrayMap(current.contents, arr, cell, f)
    }
  }
  arr
}

let keysToArray = h => linear(h, x => x.key)
let valuesToArray = h => linear(h, x => x.value)
let toArray = h => linear(h, x => (x.key, x.value))
