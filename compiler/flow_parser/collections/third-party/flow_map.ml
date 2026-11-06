(* Portions Copyright (c) Meta Platforms, Inc. and affiliates. *)

(***********************************************************************
 *                                                                     *
 *                           Objective Caml                            *
 *                                                                     *
 *            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
 *                                                                     *
 *  Copyright 1996 Institut National de Recherche en Informatique et   *
 *  en Automatique.  All rights reserved.  This file is distributed    *
 *  under the terms of the GNU Library General Public License, with    *
 *  the special exception on linking described in file LICENSE.        *
 *                                                                     *
 ***********************************************************************)

(* This module has been inspired from the OCaml standard library.
 * There are some modifications to make it run fast.
 * - It adds a Leaf node to avoid excessive allocation for singleton map
 * - In the hot [bal] function when we know it has to be [Node], we do
 *   an unsafe cast to avoid some unneeded tests
 * - Functions not needing comparison functions are lifted outside functors
 * - Leaf node is cast as a tuple to save some allocations
 * - We add some utilities e.g, [adjust] and can add more relying on the
 *   internals in the future
 *)

type ('k, 'v) t0 =
  | Empty
  | Leaf of {
      v: 'k;
      d: 'v;
    }
  | Node of {
      h: int;
      v: 'k;
      d: 'v;
      l: ('k, 'v) t0;
      r: ('k, 'v) t0;
    }

type ('k, 'v) partial_node = {
  h: int; [@live]
      (* Live: keeps the layout of the node this record is cast from with
         [~!]. *)
  v: 'k;
  d: 'v;
  l: ('k, 'v) t0;
  r: ('k, 'v) t0;
}

type ('k, 'v) leaf_tuple = 'k * 'v

external ( ~!! ) : ('k, 'v) t0 -> ('k, 'v) leaf_tuple = "%identity"

external ( ~! ) : ('k, 'v) t0 -> ('k, 'v) partial_node = "%identity"

let[@inline] height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { h; _ } -> h

let singleton x d = Leaf { v = x; d }

let sorted_two_nodes_larger node v d = Node { l = node; v; d; r = Empty; h = 2 }

let sorted_two_nodes_smaller v d node = Node { l = Empty; v; d; r = node; h = 2 }

let create l x d r =
  let hl = height l in
  let hr = height r in
  let h =
    if hl >= hr then
      hl + 1
    else
      hr + 1
  in
  if h = 1 then
    singleton x d
  else
    Node { l; v = x; d; r; h }

(* The result can not be leaf *)
let node l x d r =
  let hl = height l in
  let hr = height r in
  let h =
    if hl >= hr then
      hl + 1
    else
      hr + 1
  in
  Node { l; v = x; d; r; h }

let bal l x d r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    let { l = ll; v = lv; d = ld; r = lr; _ } = ~!l in
    if height ll >= height lr then
      node ll lv ld (create lr x d r)
    else
      let { l = lrl; v = lrv; d = lrd; r = lrr; _ } = ~!lr in
      node (create ll lv ld lrl) lrv lrd (create lrr x d r)
  else if hr > hl + 2 then
    let { l = rl; v = rv; d = rd; r = rr; _ } = ~!r in
    if height rr >= height rl then
      node (create l x d rl) rv rd rr
    else
      let { l = rll; v = rlv; d = rld; r = rlr; _ } = ~!rl in
      node (create l x d rll) rlv rld (create rlr rv rd rr)
  else
    create l x d r

let empty = Empty


(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

type ('k, 'v) t1 = ('k, 'v) t0 =
  | Empty
  | Leaf of {
      v: 'k;
      d: 'v;
    }
  | Node of {
      h: int;
      v: 'k;
      d: 'v;
      l: ('k, 'v) t0;
      r: ('k, 'v) t0;
    }

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  (* val equal : t -> t -> bool *)
end

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val find_opt : key -> 'a t -> 'a option

end

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  type key = Ord.t

  type 'a t = (key, 'a) t1

  let rec add x data m =
    match m with
    | Empty -> singleton x data
    | Leaf { v; d } ->
      let c = Ord.compare x v in
      if c = 0 then
        if d == data then
          m
        else
          Leaf { v; d = data }
      else if c < 0 then
        sorted_two_nodes_smaller x data m
      else
        sorted_two_nodes_larger m x data
    | Node { l; v; d; r; h } as m ->
      let c = Ord.compare x v in
      if c = 0 then
        if d == data then
          m
        else
          Node { l; v = x; d = data; r; h }
      else if c < 0 then
        let ll = add x data l in
        if l == ll then
          m
        else
          bal ll v d r
      else
        let rr = add x data r in
        if r == rr then
          m
        else
          bal l v d rr

  let rec find_opt x = function
    | Empty -> None
    | Leaf { v; d } ->
      let c = Ord.compare x v in
      if c = 0 then
        Some d
      else
        None
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        Some d
      else
        find_opt
          x
          ( if c < 0 then
            l
          else
            r
          )

  let empty = empty

end
