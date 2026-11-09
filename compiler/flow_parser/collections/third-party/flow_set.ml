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
 * - It adds a Leaf node to avoid excessive allocation for singleton set
 * - In the hot [bal] function when we we know it has to be [Node], we do
 *   an unsafe cast to avoid some unneeded tests
 * - Functions not need comparison functions are lifted outside functors
 * - We can add more utilities relying on the internals in the future
 *)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt

  type t

  val empty : t

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

end

type 'elt t0 =
  | Empty
  | Leaf of 'elt
  | Node of {
      h: int;
      v: 'elt;
      l: 'elt t0;
      r: 'elt t0;
    }

type 'elt partial_node = {
  h: int; [@live]
      (* Live: keeps the layout of the node this record is cast from with
         [~!]. *)
  v: 'elt;
  l: 'elt t0;
  r: 'elt t0;
}

external ( ~! ) : 'elt t0 -> 'elt partial_node = "%identity"


let[@inline] height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { h; _ } -> h

let[@inline] singleton x = Leaf x

(* FIXME: we should check to avoid creating unneeded Node
   - node
   - Node
   This function produce Node of height at least [1]
*)
let unsafe_node ~l ~v ~r =
  match (l, r) with
  | (Empty, Empty) -> singleton v
  | (Leaf _, Empty)
  | (Leaf _, Leaf _)
  | (Empty, Leaf _) ->
    Node { l; v; r; h = 2 }
  | (Node { h; _ }, (Leaf _ | Empty))
  | ((Leaf _ | Empty), Node { h; _ }) ->
    Node { l; v; r; h = h + 1 }
  | (Node { h = hl; _ }, Node { h = hr; _ }) ->
    let h =
      if hl >= hr then
        hl + 1
      else
        hr + 1
    in
    Node { l; v; r; h }

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = height l in
  let hr = height r in
  Node
    {
      l;
      v;
      r;
      h =
        ( if hl >= hr then
          hl + 1
        else
          hr + 1
        );
    }

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    (* hl is at least of height > 2 [3], so it should be [Node]
       Note having in-efficient nodes like [Node (empty,v,empty)] won't affect
       correctness here, since it will be even more likely to be [Node]
       But we are stricter with height
    *)
    let { l = ll; v = lv; r = lr; _ } = ~!l in
    if height ll >= height lr then
      create ll lv (unsafe_node ~l:lr ~v ~r)
    else
      (* Int his path hlr > hll while hl = hlr + 1 so [hlr] > 1, so it should be [Node]*)
      let { l = lrl; v = lrv; r = lrr; _ } = ~!lr in
      create (unsafe_node ~l:ll ~v:lv ~r:lrl) lrv (unsafe_node ~l:lrr ~v ~r)
  else if hr > hl + 2 then
    (* hr is at least of height > 2 [3], so it should be [Node] *)
    let { l = rl; v = rv; r = rr; _ } = ~!r in
    if height rr >= height rl then
      create (unsafe_node ~l ~v ~r:rl) rv rr
    else
      (* In this path hrl > hrr while hr = hrl + 1, so [hrl] > 1, so it should be [Node] *)
      let { l = rll; v = rlv; r = rlr; _ } = ~!rl in
      create (unsafe_node ~l ~v ~r:rll) rlv (unsafe_node ~l:rlr ~v:rv ~r:rr)
  else
    unsafe_node ~l ~v ~r

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

(* Smallest and greatest element of a set *)

(* Remove the smallest element of the given set *)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let empty = Empty

type 'a t1 = 'a t0 = private
  | Empty
  | Leaf of 'a
  | Node of {
      h: int;
      v: 'a;
      l: 'a t0;
      r: 'a t0;
    }

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  type t = elt t1

  let singleton = singleton

  let empty = empty

  let rec add x t =
    match t with
    | Empty -> singleton x
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        unsafe_node ~l:(singleton x) ~v ~r:empty
      else
        unsafe_node ~l:t ~v:x ~r:empty
    | Node { l; v; r; _ } as t ->
      let c = Ord.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let ll = add x l in
        if l == ll then
          t
        else
          bal ll v r
      else
        let rr = add x r in
        if r == rr then
          t
        else
          bal l v rr

  (* Splitting.  split x s returns a triple (l, present, r) where
      - l is the set of elements of s that are < x
      - r is the set of elements of s that are > x
      - present is false if s contains no element equal to x,
        or true if s contains an element equal to x. *)

  (* Implementation of the set operations *)

  let rec mem x = function
    | Empty -> false
    | Leaf v ->
      let c = Ord.compare x v in
      c = 0
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      c = 0
      || mem
           x
           ( if c < 0 then
             l
           else
             r
           )

end
