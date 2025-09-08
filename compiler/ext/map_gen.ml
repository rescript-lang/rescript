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
(* adapted from stdlib *)

type ('key, 'a) t0 =
  | Empty
  | Leaf of {k: 'key; v: 'a}
  | Node of {l: ('key, 'a) t0; k: 'key; v: 'a; r: ('key, 'a) t0; h: int}

type ('key, 'a) parital_node = {
  l: ('key, 'a) t0;
  k: 'key;
  v: 'a;
  r: ('key, 'a) t0;
  h: int;
}

external ( ~! ) : ('key, 'a) t0 -> ('key, 'a) parital_node = "%identity"

let empty = Empty

let rec map x f =
  match x with
  | Empty -> Empty
  | Leaf {k; v} -> Leaf {k; v = f v}
  | Node ({l; v; r} as x) ->
    let l' = map l f in
    let d' = f v in
    let r' = map r f in
    Node {x with l = l'; v = d'; r = r'}

let rec mapi x f =
  match x with
  | Empty -> Empty
  | Leaf {k; v} -> Leaf {k; v = f k v}
  | Node ({l; k; v; r} as x) ->
    let l' = mapi l f in
    let v' = f k v in
    let r' = mapi r f in
    Node {x with l = l'; v = v'; r = r'}

let[@inline] calc_height a b = (if a >= b then a else b) + 1

let[@inline] singleton k v = Leaf {k; v}

let[@inline] height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node {h} -> h

let[@inline] unsafe_node k v l r h = Node {l; k; v; r; h}

let[@inline] unsafe_two_elements k1 v1 k2 v2 =
  unsafe_node k2 v2 (singleton k1 v1) empty 2

let[@inline] unsafe_node_maybe_leaf k v l r h =
  if h = 1 then Leaf {k; v} else Node {l; k; v; r; h}

type ('key, +'a) t = ('key, 'a) t0 = private
  | Empty
  | Leaf of {k: 'key; v: 'a}
  | Node of {l: ('key, 'a) t; k: 'key; v: 'a; r: ('key, 'a) t; h: int}

let rec cardinal_aux acc = function
  | Empty -> acc
  | Leaf _ -> acc + 1
  | Node {l; r} -> cardinal_aux (cardinal_aux (acc + 1) r) l

let cardinal s = cardinal_aux 0 s

let rec bindings_aux accu = function
  | Empty -> accu
  | Leaf {k; v} -> (k, v) :: accu
  | Node {l; k; v; r} -> bindings_aux ((k, v) :: bindings_aux accu r) l

let bindings s = bindings_aux [] s

let rec fill_array_with_f (s : _ t) i arr f : int =
  match s with
  | Empty -> i
  | Leaf {k; v} ->
    Array.unsafe_set arr i (f k v);
    i + 1
  | Node {l; k; v; r} ->
    let inext = fill_array_with_f l i arr f in
    Array.unsafe_set arr inext (f k v);
    fill_array_with_f r (inext + 1) arr f

let rec fill_array_aux (s : _ t) i arr : int =
  match s with
  | Empty -> i
  | Leaf {k; v} ->
    Array.unsafe_set arr i (k, v);
    i + 1
  | Node {l; k; v; r} ->
    let inext = fill_array_aux l i arr in
    Array.unsafe_set arr inext (k, v);
    fill_array_aux r (inext + 1) arr

let to_sorted_array (s : ('key, 'a) t) : ('key * 'a) array =
  match s with
  | Empty -> [||]
  | Leaf {k; v} -> [|(k, v)|]
  | Node {l; k; v; r} ->
    let len = cardinal_aux (cardinal_aux 1 r) l in
    let arr = Array.make len (k, v) in
    ignore (fill_array_aux s 0 arr : int);
    arr

let to_sorted_array_with_f (type key a b) (s : (key, a) t) (f : key -> a -> b) :
    b array =
  match s with
  | Empty -> [||]
  | Leaf {k; v} -> [|f k v|]
  | Node {l; k; v; r} ->
    let len = cardinal_aux (cardinal_aux 1 r) l in
    let arr = Array.make len (f k v) in
    ignore (fill_array_with_f s 0 arr f : int);
    arr

let rec keys_aux accu = function
  | Empty -> accu
  | Leaf {k} -> k :: accu
  | Node {l; k; r} -> keys_aux (k :: keys_aux accu r) l

let keys s = keys_aux [] s

let bal l x d r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    let {l = ll; r = lr; v = lv; k = lk; h = _} = ~!l in
    let hll = height ll in
    let hlr = height lr in
    if hll >= hlr then
      let hnode = calc_height hlr hr in
      unsafe_node lk lv ll
        (unsafe_node_maybe_leaf x d lr r hnode)
        (calc_height hll hnode)
    else
      let {l = lrl; r = lrr; k = lrk; v = lrv} = ~!lr in
      let hlrl = height lrl in
      let hlrr = height lrr in
      let hlnode = calc_height hll hlrl in
      let hrnode = calc_height hlrr hr in
      unsafe_node lrk lrv
        (unsafe_node_maybe_leaf lk lv ll lrl hlnode)
        (unsafe_node_maybe_leaf x d lrr r hrnode)
        (calc_height hlnode hrnode)
  else if hr > hl + 2 then
    let {l = rl; r = rr; k = rk; v = rv} = ~!r in
    let hrr = height rr in
    let hrl = height rl in
    if hrr >= hrl then
      let hnode = calc_height hl hrl in
      unsafe_node rk rv
        (unsafe_node_maybe_leaf x d l rl hnode)
        rr (calc_height hnode hrr)
    else
      let {l = rll; r = rlr; k = rlk; v = rlv} = ~!rl in
      let hrll = height rll in
      let hrlr = height rlr in
      let hlnode = calc_height hl hrll in
      let hrnode = calc_height hrlr hrr in
      unsafe_node rlk rlv
        (unsafe_node_maybe_leaf x d l rll hlnode)
        (unsafe_node_maybe_leaf rk rv rlr rr hrnode)
        (calc_height hlnode hrnode)
  else unsafe_node_maybe_leaf x d l r (calc_height hl hr)

let[@inline] is_empty = function
  | Empty -> true
  | _ -> false

let rec min_binding_exn = function
  | Empty -> raise Not_found
  | Leaf {k; v} -> (k, v)
  | Node {l; k; v} -> (
    match l with
    | Empty -> (k, v)
    | Leaf _ | Node _ -> min_binding_exn l)

let rec remove_min_binding = function
  | Empty -> invalid_arg "Map.remove_min_elt"
  | Leaf _ -> empty
  | Node {l = Empty; r} -> r
  | Node {l; k; v; r} -> bal (remove_min_binding l) k v r

let merge t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ ->
    let x, d = min_binding_exn t2 in
    bal t1 x d (remove_min_binding t2)

let rec iter x f =
  match x with
  | Empty -> ()
  | Leaf {k; v} -> (f k v : unit)
  | Node {l; k; v; r} ->
    iter l f;
    f k v;
    iter r f

let rec fold m accu f =
  match m with
  | Empty -> accu
  | Leaf {k; v} -> f k v accu
  | Node {l; k; v; r} -> fold r (f k v (fold l accu f)) f

let rec for_all x p =
  match x with
  | Empty -> true
  | Leaf {k; v} -> p k v
  | Node {l; k; v; r} -> p k v && for_all l p && for_all r p

let rec exists x p =
  match x with
  | Empty -> false
  | Leaf {k; v} -> p k v
  | Node {l; k; v; r} -> p k v || exists l p || exists r p

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min k v = function
  | Empty -> singleton k v
  | Leaf l -> unsafe_two_elements k v l.k l.v
  | Node tree -> bal (add_min k v tree.l) tree.k tree.v tree.r

let rec add_max k v = function
  | Empty -> singleton k v
  | Leaf l -> unsafe_two_elements l.k l.v k v
  | Node tree -> bal tree.l tree.k tree.v (add_max k v tree.r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match l with
  | Empty -> add_min v d r
  | Leaf leaf -> add_min leaf.k leaf.v (add_min v d r)
  | Node xl -> (
    match r with
    | Empty -> add_max v d l
    | Leaf leaf -> add_max leaf.k leaf.v (add_max v d l)
    | Node xr ->
      let lh = xl.h in
      let rh = xr.h in
      if lh > rh + 2 then bal xl.l xl.k xl.v (join xl.r v d r)
      else if rh > lh + 2 then bal (join l v d xr.l) xr.k xr.v xr.r
      else unsafe_node v d l r (calc_height lh rh))

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
  | Empty, t -> t
  | t, Empty -> t
  | _, _ ->
    let x, d = min_binding_exn t2 in
    join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val compare_key : key -> key -> int

  val is_empty : 'a t -> bool

  val mem : 'a t -> key -> bool

  val to_sorted_array : 'a t -> (key * 'a) array

  val to_sorted_array_with_f : 'a t -> (key -> 'a -> 'b) -> 'b array

  val add : 'a t -> key -> 'a -> 'a t
  (** [add x y m] 
      If [x] was already bound in [m], its previous binding disappears. *)

  val adjust : 'a t -> key -> ('a option -> 'a) -> 'a t
  (** [adjust acc k replace ] if not exist [add (replace None ], otherwise 
      [add k v (replace (Some old))]
  *)

  val singleton : key -> 'a -> 'a t

  val remove : 'a t -> key -> 'a t
  (** [remove x m] returns a map containing the same bindings as
      [m], except for [x] which is unbound in the returned map. *)

  (* val merge:
       'a t -> 'b t ->
       (key -> 'a option -> 'b option -> 'c option) ->  'c t *)
  (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
      and of [m2]. The presence of each such binding, and the corresponding
      value, is determined with the function [f].
      @since 3.12.0
  *)

  val disjoint_merge_exn : 'a t -> 'a t -> (key -> 'a -> 'a -> exn) -> 'a t
  (* merge two maps, will raise if they have the same key *)

  val iter : 'a t -> (key -> 'a -> unit) -> unit
  (** [iter f m] applies [f] to all bindings in map [m].
      The bindings are passed to [f] in increasing order. *)

  val fold : 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b
  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
      where [k1 ... kN] are the keys of all bindings in [m]
      (in increasing order) *)

  val for_all : 'a t -> (key -> 'a -> bool) -> bool
  (** [for_all p m] checks if all the bindings of the map.
      order unspecified
  *)

  val exists : 'a t -> (key -> 'a -> bool) -> bool
  (** [exists p m] checks if at least one binding of the map
      satisfy the predicate [p]. 
      order unspecified
  *)

  (* val filter: 'a t -> (key -> 'a -> bool) -> 'a t *)
  (** [filter p m] returns the map with all the bindings in [m]
      that satisfy predicate [p].
      order unspecified
  *)

  (* val partition: 'a t -> (key -> 'a -> bool) ->  'a t * 'a t *)
  (** [partition p m] returns a pair of maps [(m1, m2)], where
      [m1] contains all the bindings of [s] that satisfy the
      predicate [p], and [m2] is the map with all the bindings of
      [s] that do not satisfy [p].
  *)

  val cardinal : 'a t -> int
  (** Return the number of bindings of a map. *)

  val bindings : 'a t -> (key * 'a) list
  (** Return the list of all bindings of the given map.
      The returned list is sorted in increasing order with respect
      to the ordering *)

  val keys : 'a t -> key list
  (* Increasing order *)

  (* val split: 'a t -> key -> 'a t * 'a option * 'a t *)
  (** [split x m] returns a triple [(l, data, r)], where
        [l] is the map with all the bindings of [m] whose key
      is strictly less than [x];
        [r] is the map with all the bindings of [m] whose key
      is strictly greater than [x];
        [data] is [None] if [m] contains no binding for [x],
        or [Some v] if [m] binds [v] to [x].
      @since 3.12.0
  *)

  val find_exn : 'a t -> key -> 'a
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val find_opt : 'a t -> key -> 'a option

  val find_default : 'a t -> key -> 'a -> 'a

  val map : 'a t -> ('a -> 'b) -> 'b t
  (** [map f m] returns a map with same domain as [m], where the
      associated value [a] of all bindings of [m] has been
      replaced by the result of the application of [f] to [a].
      The bindings are passed to [f] in increasing order
      with respect to the ordering over the type of the keys. *)

  val mapi : 'a t -> (key -> 'a -> 'b) -> 'b t
  (** Same as {!Map.S.map}, but the function receives as arguments both the
      key and the associated value for each binding of the map. *)

  val of_list : (key * 'a) list -> 'a t

  val of_array : (key * 'a) array -> 'a t

  val add_list : (key * 'b) list -> 'b t -> 'b t
end
