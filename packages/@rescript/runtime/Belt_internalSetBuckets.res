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

/* We do dynamic hashing, and resize the table and rehash the elements
 when buckets become too long. */
module C = Belt_internalBucketsType
/* TODO:
   the current implementation relies on the fact that bucket 
   empty value is `undefined` in both places,
   in theory, it can be different 

*/
type rec bucket<'a> = {
  mutable key: 'a,
  mutable next: C.opt<bucket<'a>>,
}
and t<'hash, 'eq, 'a> = C.container<'hash, 'eq, bucket<'a>>

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
      next: C.emptyOpt,
    }
    copyAuxCont(c.next, head)
    C.return(head)
  }
and copyAuxCont = (c, prec) =>
  switch C.toOpt(c) {
  | None => ()
  | Some(nc) =>
    let ncopy = {key: nc.key, next: C.emptyOpt}
    prec.next = C.return(ncopy)
    copyAuxCont(nc.next, ncopy)
  }

let rec bucketLength = (accu, buckets) =>
  switch C.toOpt(buckets) {
  | None => accu
  | Some(cell) => bucketLength(accu + 1, cell.next)
  }

let rec doBucketIter = (~f, buckets) =>
  switch C.toOpt(buckets) {
  | None => ()
  | Some(cell) =>
    f(cell.key)
    doBucketIter(~f, cell.next)
  }

let forEach = (h, f) => {
  let d = h.C.buckets
  for i in 0 to A.length(d) - 1 {
    doBucketIter(~f, A.getUnsafe(d, i))
  }
}

let rec fillArray = (i, arr, cell) => {
  A.setUnsafe(arr, i, cell.key)
  switch C.toOpt(cell.next) {
  | None => i + 1
  | Some(v) => fillArray(i + 1, arr, v)
  }
}

let toArray = h => {
  let d = h.C.buckets
  let current = ref(0)
  let arr = A.makeUninitializedUnsafe(h.C.size)
  for i in 0 to A.length(d) - 1 {
    let cell = A.getUnsafe(d, i)
    switch C.toOpt(cell) {
    | None => ()
    | Some(cell) => current.contents = fillArray(current.contents, arr, cell)
    }
  }
  arr
}

let rec doBucketFold = (~f, b, accu) =>
  switch C.toOpt(b) {
  | None => accu
  | Some(cell) => doBucketFold(~f, cell.next, f(accu, cell.key))
  }

let reduce = (h, init, f) => {
  let d = h.C.buckets
  let accu = ref(init)
  for i in 0 to A.length(d) - 1 {
    accu.contents = doBucketFold(~f, A.getUnsafe(d, i), accu.contents)
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
