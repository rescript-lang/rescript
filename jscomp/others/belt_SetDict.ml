(* Copyright (C) 2017 Authors of BuckleScript
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

module N = Belt_internalAVLset
module A = Belt_Array

type ('k, 'id) t = 'k N.t
type ('key, 'id) cmp = ('key, 'id) Belt_Id.cmp

(* here we relies on reference transparence address equality means everything
   equal across time no need to call [bal] again *)
let rec add (t : _ t) x ~cmp : _ t =
  match N.toOpt t with
  | None -> N.singleton x
  | Some nt ->
      let k = N.valueGet nt in
      let c = ((Belt_Id.getCmpInternal cmp) x k [@bs]) in
      if c = 0 then t
      else
        let l, r = N.(leftGet nt, rightGet nt) in
        if c < 0 then
          let ll = add ~cmp l x in
          if ll == l then t else N.bal ll k r
        else
          let rr = add ~cmp r x in
          if rr == r then t else N.bal l k rr

let rec remove (t : _ t) x ~cmp : _ t =
  match N.toOpt t with
  | None -> t
  | Some n ->
      let l, v, r = N.(leftGet n, valueGet n, rightGet n) in
      let c = ((Belt_Id.getCmpInternal cmp) x v [@bs]) in
      if c = 0 then
        match (N.toOpt l, N.toOpt r) with
        | None, _ -> r
        | _, None -> l
        | _, Some rn ->
            let v = ref (N.valueGet rn) in
            let r = N.removeMinAuxWithRef rn v in
            N.bal l !v r
      else if c < 0 then
        let ll = remove ~cmp l x in
        if ll == l then t else N.bal ll v r
      else
        let rr = remove ~cmp r x in
        if rr == r then t else N.bal l v rr

let mergeMany h arr ~cmp =
  let len = A.length arr in
  let v = ref h in
  for i = 0 to len - 1 do
    let key = A.getUnsafe arr i in
    v := add !v ~cmp key
  done ;
  !v

let removeMany h arr ~cmp =
  let len = A.length arr in
  let v = ref h in
  for i = 0 to len - 1 do
    let key = A.getUnsafe arr i in
    v := remove !v ~cmp key
  done ;
  !v

let rec splitAuxNoPivot ~cmp (n : _ N.node) x : _ * _ =
  let l, v, r = N.(leftGet n, valueGet n, rightGet n) in
  let c = ((Belt_Id.getCmpInternal cmp) x v [@bs]) in
  if c = 0 then (l, r)
  else if c < 0 then
    match N.toOpt l with
    | None -> (N.empty, N.return n)
    | Some l ->
        let ll, rl = splitAuxNoPivot ~cmp l x in
        (ll, N.joinShared rl v r)
  else
    match N.toOpt r with
    | None -> (N.return n, N.empty)
    | Some r ->
        let lr, rr = splitAuxNoPivot ~cmp r x in
        (N.joinShared l v lr, rr)

let rec splitAuxPivot ~cmp (n : _ N.node) x pres : _ * _ =
  let l, v, r = N.(leftGet n, valueGet n, rightGet n) in
  let c = ((Belt_Id.getCmpInternal cmp) x v [@bs]) in
  if c = 0 then (
    pres := true ;
    (l, r) )
  else if c < 0 then
    match N.toOpt l with
    | None -> (N.empty, N.return n)
    | Some l ->
        let ll, rl = splitAuxPivot ~cmp l x pres in
        (ll, N.joinShared rl v r)
  else
    match N.toOpt r with
    | None -> (N.return n, N.empty)
    | Some r ->
        let lr, rr = splitAuxPivot ~cmp r x pres in
        (N.joinShared l v lr, rr)

let split (t : _ t) x ~cmp =
  match N.toOpt t with
  | None -> ((N.empty, N.empty), false)
  | Some n ->
      let pres = ref false in
      let v = splitAuxPivot ~cmp n x pres in
      (v, !pres)

(* [union s1 s2] Use the pivot to split the smaller collection *)
let rec union (s1 : _ t) (s2 : _ t) ~cmp : _ t =
  match N.(toOpt s1, toOpt s2) with
  | None, _ -> s2
  | _, None -> s1
  | Some n1, Some n2 ->
      let h1, h2 = N.(heightGet n1, heightGet n2) in
      if h1 >= h2 then
        if h2 = 1 then add ~cmp s1 (N.valueGet n2)
        else
          let l1, v1, r1 = N.(leftGet n1, valueGet n1, rightGet n1) in
          let l2, r2 = splitAuxNoPivot ~cmp n2 v1 in
          N.joinShared (union ~cmp l1 l2) v1 (union ~cmp r1 r2)
      else if h1 = 1 then add s2 ~cmp (N.valueGet n1)
      else
        let l2, v2, r2 = N.(leftGet n2, valueGet n2, rightGet n2) in
        let l1, r1 = splitAuxNoPivot ~cmp n1 v2 in
        N.joinShared (union ~cmp l1 l2) v2 (union ~cmp r1 r2)

let rec intersect (s1 : _ t) (s2 : _ t) ~cmp =
  match N.(toOpt s1, toOpt s2) with
  | None, _ | _, None -> N.empty
  | Some n1, Some n2 ->
      let l1, v1, r1 = N.(leftGet n1, valueGet n1, rightGet n1) in
      let pres = ref false in
      let l2, r2 = splitAuxPivot ~cmp n2 v1 pres in
      let ll = intersect ~cmp l1 l2 in
      let rr = intersect ~cmp r1 r2 in
      if !pres then N.joinShared ll v1 rr else N.concatShared ll rr

let rec diff s1 s2 ~cmp =
  match N.(toOpt s1, toOpt s2) with
  | None, _ | _, None -> s1
  | Some n1, Some n2 ->
      let l1, v1, r1 = N.(leftGet n1, valueGet n1, rightGet n1) in
      let pres = ref false in
      let l2, r2 = splitAuxPivot ~cmp n2 v1 pres in
      let ll = diff ~cmp l1 l2 in
      let rr = diff ~cmp r1 r2 in
      if !pres then N.concatShared ll rr else N.joinShared ll v1 rr

let empty = N.empty
let fromArray = N.fromArray
let isEmpty = N.isEmpty
let cmp = N.cmp
let eq = N.eq
let has = N.has
let forEachU = N.forEachU
let forEach = N.forEach
let reduceU = N.reduceU
let reduce = N.reduce
let everyU = N.everyU
let every = N.every
let someU = N.someU
let some = N.some
let size = N.size
let toList = N.toList
let toArray = N.toArray
let minimum = N.minimum
let maximum = N.maximum
let maxUndefined = N.maxUndefined
let minUndefined = N.minUndefined
let get = N.get
let getExn = N.getExn
let getUndefined = N.getUndefined
let fromSortedArrayUnsafe = N.fromSortedArrayUnsafe
let subset = N.subset
let keep = N.keepShared
let keepU = N.keepSharedU
let partitionU = N.partitionSharedU
let partition = N.partitionShared
let checkInvariantInternal = N.checkInvariantInternal
