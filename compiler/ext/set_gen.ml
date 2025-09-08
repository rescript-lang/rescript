(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
[@@@warnerror "+55"]

(* balanced tree based on stdlib distribution *)

type 'a t0 = Empty | Leaf of 'a | Node of {l: 'a t0; v: 'a; r: 'a t0; h: int}

type 'a partial_node = {l: 'a t0; v: 'a; r: 'a t0; h: int}

external ( ~! ) : 'a t0 -> 'a partial_node = "%identity"

let empty = Empty

let[@inline] height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node {h} -> h

let[@inline] calc_height a b = (if a >= b then a else b) + 1

(*
     Invariants:
     1. {[ l < v < r]}
     2. l and r balanced
     3. [height l] - [height r] <= 2
*)
let[@inline] unsafe_node v l r h = Node {l; v; r; h}

let[@inline] unsafe_node_maybe_leaf v l r h =
  if h = 1 then Leaf v else Node {l; v; r; h}

let[@inline] singleton x = Leaf x

let[@inline] unsafe_two_elements x v = unsafe_node v (singleton x) empty 2

type 'a t = 'a t0 = private
  | Empty
  | Leaf of 'a
  | Node of {l: 'a t0; v: 'a; r: 'a t0; h: int}

(* Smallest and greatest element of a set *)

let rec min_exn = function
  | Empty -> raise Not_found
  | Leaf v -> v
  | Node {l; v} -> (
    match l with
    | Empty -> v
    | Leaf _ | Node _ -> min_exn l)

let[@inline] is_empty = function
  | Empty -> true
  | _ -> false

let rec cardinal_aux acc = function
  | Empty -> acc
  | Leaf _ -> acc + 1
  | Node {l; r} -> cardinal_aux (cardinal_aux (acc + 1) r) l

let cardinal s = cardinal_aux 0 s

let rec elements_aux accu = function
  | Empty -> accu
  | Leaf v -> v :: accu
  | Node {l; v; r} -> elements_aux (v :: elements_aux accu r) l

let elements s = elements_aux [] s

let choose = min_exn

let rec iter x f =
  match x with
  | Empty -> ()
  | Leaf v -> f v
  | Node {l; v; r} ->
    iter l f;
    f v;
    iter r f

let rec fold s accu f =
  match s with
  | Empty -> accu
  | Leaf v -> f v accu
  | Node {l; v; r} -> fold r (f v (fold l accu f)) f

let rec for_all x p =
  match x with
  | Empty -> true
  | Leaf v -> p v
  | Node {l; v; r} -> p v && for_all l p && for_all r p

let rec exists x p =
  match x with
  | Empty -> false
  | Leaf v -> p v
  | Node {l; v; r} -> p v || exists l p || exists r p

exception Height_invariant_broken

exception Height_diff_borken

let rec check_height_and_diff = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node {l; r; h} ->
    let hl = check_height_and_diff l in
    let hr = check_height_and_diff r in
    if h <> calc_height hl hr then raise Height_invariant_broken
    else
      let diff = abs (hl - hr) in
      if diff > 2 then raise Height_diff_borken else h

let check tree = ignore (check_height_and_diff tree)

(* Same as create, but performs one step of rebalancing if necessary.
    Invariants:
    1. {[ l < v < r ]}
    2. l and r balanced
    3. | height l - height r | <= 3.

    Proof by indunction

    Lemma: the height of  [bal l v r] will bounded by [max l r] + 1
*)
let bal l v r : _ t =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    let {l = ll; r = lr; v = lv; h = _} = ~!l in
    let hll = height ll in
    let hlr = height lr in
    if hll >= hlr then
      let hnode = calc_height hlr hr in
      unsafe_node lv ll
        (unsafe_node_maybe_leaf v lr r hnode)
        (calc_height hll hnode)
    else
      let {l = lrl; r = lrr; v = lrv} = ~!lr in
      let hlrl = height lrl in
      let hlrr = height lrr in
      let hlnode = calc_height hll hlrl in
      let hrnode = calc_height hlrr hr in
      unsafe_node lrv
        (unsafe_node_maybe_leaf lv ll lrl hlnode)
        (unsafe_node_maybe_leaf v lrr r hrnode)
        (calc_height hlnode hrnode)
  else if hr > hl + 2 then
    let {l = rl; r = rr; v = rv} = ~!r in
    let hrr = height rr in
    let hrl = height rl in
    if hrr >= hrl then
      let hnode = calc_height hl hrl in
      unsafe_node rv
        (unsafe_node_maybe_leaf v l rl hnode)
        rr (calc_height hnode hrr)
    else
      let {l = rll; r = rlr; v = rlv} = ~!rl in
      let hrll = height rll in
      let hrlr = height rlr in
      let hlnode = calc_height hl hrll in
      let hrnode = calc_height hrlr hrr in
      unsafe_node rlv
        (unsafe_node_maybe_leaf v l rll hlnode)
        (unsafe_node_maybe_leaf rv rlr rr hrnode)
        (calc_height hlnode hrnode)
  else unsafe_node_maybe_leaf v l r (calc_height hl hr)

let rec remove_min_elt = function
  | Empty -> invalid_arg "Set.remove_min_elt"
  | Leaf _ -> empty
  | Node {l = Empty; r} -> r
  | Node {l; v; r} -> bal (remove_min_elt l) v r

(*
    All elements of l must precede the elements of r.
        Assume | height l - height r | <= 2.
    weak form of [concat]
*)

let internal_merge l r =
  match (l, r) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ -> bal l (min_exn r) (remove_min_elt r)

(* Beware: those two functions assume that the added v is *strictly*
    smaller (or bigger) than all the present elements in the tree; it
    does not test for equality with the current min (or max) element.
    Indeed, they are only used during the "join" operation which
    respects this precondition.
*)

let rec add_min v = function
  | Empty -> singleton v
  | Leaf x -> unsafe_two_elements v x
  | Node n -> bal (add_min v n.l) n.v n.r

let rec add_max v = function
  | Empty -> singleton v
  | Leaf x -> unsafe_two_elements x v
  | Node n -> bal n.l n.v (add_max v n.r)

(** 
    Invariants:
    1. l < v < r 
    2. l and r are balanced 

    Proof by induction
    The height of output will be ~~ (max (height l) (height r) + 2)
    Also use the lemma from [bal]
*)
let rec internal_join l v r =
  match (l, r) with
  | Empty, _ -> add_min v r
  | _, Empty -> add_max v l
  | Leaf lv, Node {h = rh} ->
    if rh > 3 then add_min lv (add_min v r) (* FIXME: could inlined *)
    else unsafe_node v l r (rh + 1)
  | Leaf _, Leaf _ -> unsafe_node v l r 2
  | Node {h = lh}, Leaf rv ->
    if lh > 3 then add_max rv (add_max v l) else unsafe_node v l r (lh + 1)
  | Node {l = ll; v = lv; r = lr; h = lh}, Node {l = rl; v = rv; r = rr; h = rh}
    ->
    if lh > rh + 2 then
      (* proof by induction:
         now [height of ll] is [lh - 1]
      *)
      bal ll lv (internal_join lr v r)
    else if rh > lh + 2 then bal (internal_join l v rl) rv rr
    else unsafe_node v l r (calc_height lh rh)

(*
    Required Invariants: 
    [t1] < [t2]  
*)
let internal_concat t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ -> internal_join t1 (min_exn t2) (remove_min_elt t2)

let rec partition x p =
  match x with
  | Empty -> (empty, empty)
  | Leaf v ->
    let pv = p v in
    if pv then (x, empty) else (empty, x)
  | Node {l; v; r} ->
    (* call [p] in the expected left-to-right order *)
    let lt, lf = partition l p in
    let pv = p v in
    let rt, rf = partition r p in
    if pv then (internal_join lt v rt, internal_concat lf rf)
    else (internal_concat lt rt, internal_join lf v rf)

let of_sorted_array l =
  let rec sub start n l =
    if n = 0 then empty
    else if n = 1 then
      let x0 = Array.unsafe_get l start in
      singleton x0
    else if n = 2 then
      let x0 = Array.unsafe_get l start in
      let x1 = Array.unsafe_get l (start + 1) in
      unsafe_node x1 (singleton x0) empty 2
    else if n = 3 then
      let x0 = Array.unsafe_get l start in
      let x1 = Array.unsafe_get l (start + 1) in
      let x2 = Array.unsafe_get l (start + 2) in
      unsafe_node x1 (singleton x0) (singleton x2) 2
    else
      let nl = n / 2 in
      let left = sub start nl l in
      let mid = start + nl in
      let v = Array.unsafe_get l mid in
      let right = sub (mid + 1) (n - nl - 1) l in
      unsafe_node v left right (calc_height (height left) (height right))
  in
  sub 0 (Array.length l) l

let is_ordered ~cmp tree =
  let rec is_ordered_min_max tree =
    match tree with
    | Empty -> `Empty
    | Leaf v -> `V (v, v)
    | Node {l; v; r} -> (
      match is_ordered_min_max l with
      | `No -> `No
      | `Empty -> (
        match is_ordered_min_max r with
        | `No -> `No
        | `Empty -> `V (v, v)
        | `V (l, r) -> if cmp v l < 0 then `V (v, r) else `No)
      | `V (min_v, max_v) -> (
        match is_ordered_min_max r with
        | `No -> `No
        | `Empty -> if cmp max_v v < 0 then `V (min_v, v) else `No
        | `V (min_v_r, max_v_r) ->
          if cmp max_v min_v_r < 0 then `V (min_v, max_v_r) else `No))
  in
  is_ordered_min_max tree <> `No

let invariant ~cmp t =
  check t;
  is_ordered ~cmp t

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> (elt -> unit) -> unit

  val fold : t -> 'a -> (elt -> 'a -> 'a) -> 'a

  val for_all : t -> (elt -> bool) -> bool

  val exists : t -> (elt -> bool) -> bool

  val singleton : elt -> t

  val cardinal : t -> int

  val elements : t -> elt list

  val choose : t -> elt

  val mem : t -> elt -> bool

  val add : t -> elt -> t

  val remove : t -> elt -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val of_list : elt list -> t

  val of_sorted_array : elt array -> t

  val invariant : t -> bool

  val print : Format.formatter -> t -> unit
end
