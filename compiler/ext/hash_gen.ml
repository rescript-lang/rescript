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

(* Hash tables *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) bucket =
  | Empty
  | Cons of {mutable key: 'a; mutable data: 'b; mutable next: ('a, 'b) bucket}

type ('a, 'b) t = {
  mutable size: int;
  (* number of entries *)
  mutable data: ('a, 'b) bucket array;
  (* the buckets *)
  initial_size: int; (* initial array size *)
}

let create initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  {initial_size = s; size = 0; data = Array.make s Empty}

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    Array.unsafe_set h.data i Empty
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then (
    let ndata = Array.make nsize Empty in
    let ndata_tail = Array.make nsize Empty in
    h.data <- ndata;
    (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
      | Empty -> ()
      | Cons {key; next} as cell ->
        let nidx = indexfun h key in
        (match Array.unsafe_get ndata_tail nidx with
        | Empty -> Array.unsafe_set ndata nidx cell
        | Cons tail -> tail.next <- cell);
        Array.unsafe_set ndata_tail nidx cell;
        insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do
      match Array.unsafe_get ndata_tail i with
      | Empty -> ()
      | Cons tail -> tail.next <- Empty
    done)

let iter h f =
  let rec do_bucket = function
    | Empty -> ()
    | Cons l ->
      f l.key l.data;
      do_bucket l.next
  in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold h init f =
  let rec do_bucket b accu =
    match b with
    | Empty -> accu
    | Cons l -> do_bucket l.next (f l.key l.data accu)
  in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done;
  !accu

let to_list h f = fold h [] (fun k data acc -> f k data :: acc)

let rec small_bucket_mem (lst : _ bucket) eq key =
  match lst with
  | Empty -> false
  | Cons lst -> (
    eq key lst.key
    ||
    match lst.next with
    | Empty -> false
    | Cons lst -> (
      eq key lst.key
      ||
      match lst.next with
      | Empty -> false
      | Cons lst -> eq key lst.key || small_bucket_mem lst.next eq key))

let rec small_bucket_opt eq key (lst : _ bucket) : _ option =
  match lst with
  | Empty -> None
  | Cons lst -> (
    if eq key lst.key then Some lst.data
    else
      match lst.next with
      | Empty -> None
      | Cons lst -> (
        if eq key lst.key then Some lst.data
        else
          match lst.next with
          | Empty -> None
          | Cons lst ->
            if eq key lst.key then Some lst.data
            else small_bucket_opt eq key lst.next))

let rec small_bucket_key_opt eq key (lst : _ bucket) : _ option =
  match lst with
  | Empty -> None
  | Cons {key = k; next} -> (
    if eq key k then Some k
    else
      match next with
      | Empty -> None
      | Cons {key = k; next} -> (
        if eq key k then Some k
        else
          match next with
          | Empty -> None
          | Cons {key = k; next} ->
            if eq key k then Some k else small_bucket_key_opt eq key next))

let rec small_bucket_default eq key default (lst : _ bucket) =
  match lst with
  | Empty -> default
  | Cons lst -> (
    if eq key lst.key then lst.data
    else
      match lst.next with
      | Empty -> default
      | Cons lst -> (
        if eq key lst.key then lst.data
        else
          match lst.next with
          | Empty -> default
          | Cons lst ->
            if eq key lst.key then lst.data
            else small_bucket_default eq key default lst.next))

let rec remove_bucket h (i : int) key ~(prec : _ bucket) (buck : _ bucket)
    eq_key =
  match buck with
  | Empty -> ()
  | Cons {key = k; next} ->
    if eq_key k key then (
      h.size <- h.size - 1;
      match prec with
      | Empty -> Array.unsafe_set h.data i next
      | Cons c -> c.next <- next)
    else remove_bucket h i key ~prec:buck next eq_key

let rec replace_bucket key data (buck : _ bucket) eq_key =
  match buck with
  | Empty -> true
  | Cons slot ->
    if eq_key slot.key key then (
      slot.key <- key;
      slot.data <- data;
      false)
    else replace_bucket key data slot.next eq_key

module type S = sig
  type key

  type 'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val reset : 'a t -> unit

  val add : 'a t -> key -> 'a -> unit

  val add_or_update : 'a t -> key -> update:('a -> 'a) -> 'a -> unit

  val remove : 'a t -> key -> unit

  val find_exn : 'a t -> key -> 'a

  val find_all : 'a t -> key -> 'a list

  val find_opt : 'a t -> key -> 'a option

  val find_key_opt : 'a t -> key -> key option
  (** return the key found in the hashtbl.
      Use case: when you find the key existed in hashtbl, 
      you want to use the one stored in the hashtbl. 
      (they are semantically equivlanent, but may have other information different) 
  *)

  val find_default : 'a t -> key -> 'a -> 'a

  val replace : 'a t -> key -> 'a -> unit

  val mem : 'a t -> key -> bool

  val iter : 'a t -> (key -> 'a -> unit) -> unit

  val fold : 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b

  val length : 'a t -> int

  (* val stats: 'a t -> Hashtbl.statistics *)
  val to_list : 'a t -> (key -> 'a -> 'c) -> 'c list

  val of_list2 : key list -> 'a list -> 'a t
end
