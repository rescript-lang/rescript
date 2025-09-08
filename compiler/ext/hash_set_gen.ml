(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a bucket = Empty | Cons of {mutable key: 'a; mutable next: 'a bucket}

type 'a t = {
  mutable size: int;
  (* number of entries *)
  mutable data: 'a bucket array;
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
      f l.key;
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
    | Cons l -> do_bucket l.next (f l.key accu)
  in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done;
  !accu

let to_list set = fold set [] List.cons

let rec small_bucket_mem eq key lst =
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
      | Cons lst -> eq key lst.key || small_bucket_mem eq key lst.next))

let rec remove_bucket (h : _ t) (i : int) key ~(prec : _ bucket)
    (buck : _ bucket) eq_key =
  match buck with
  | Empty -> ()
  | Cons {key = k; next} ->
    if eq_key k key then (
      h.size <- h.size - 1;
      match prec with
      | Empty -> Array.unsafe_set h.data i next
      | Cons c -> c.next <- next)
    else remove_bucket h i key ~prec:buck next eq_key

module type S = sig
  type key

  type t

  val create : int -> t

  val clear : t -> unit

  val reset : t -> unit

  (* val copy: t -> t *)
  val remove : t -> key -> unit

  val add : t -> key -> unit

  val of_array : key array -> t

  val check_add : t -> key -> bool

  val mem : t -> key -> bool

  val iter : t -> (key -> unit) -> unit

  val fold : t -> 'b -> (key -> 'b -> 'b) -> 'b

  val length : t -> int

  (* val stats:  t -> Hashtbl.statistics *)
  val to_list : t -> key list
end
