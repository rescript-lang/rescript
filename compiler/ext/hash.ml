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

module Make (Key : Hashtbl.HashedType) = struct
  type key = Key.t
  type 'a t = (key, 'a) Hash_gen.t
  let key_index (h : _ t) (key : key) =
    Key.hash key land (Array.length h.data - 1)
  let eq_key = Key.equal

  type ('a, 'b) bucket = ('a, 'b) Hash_gen.bucket
  let create = Hash_gen.create
  let clear = Hash_gen.clear
  let reset = Hash_gen.reset
  let iter = Hash_gen.iter
  let to_list = Hash_gen.to_list
  let fold = Hash_gen.fold
  let length = Hash_gen.length
  (* let stats = Hash_gen.stats *)

  let add (h : _ t) key data =
    let i = key_index h key in
    let h_data = h.data in
    Array.unsafe_set h_data i
      (Cons {key; data; next = Array.unsafe_get h_data i});
    h.size <- h.size + 1;
    if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h

  (* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
  let add_or_update (h : 'a t) (key : key) ~update:(modf : 'a -> 'a)
      (default : 'a) : unit =
    let rec find_bucket (bucketlist : _ bucket) : bool =
      match bucketlist with
      | Cons rhs ->
        if eq_key rhs.key key then (
          rhs.data <- modf rhs.data;
          false)
        else find_bucket rhs.next
      | Empty -> true
    in
    let i = key_index h key in
    let h_data = h.data in
    if find_bucket (Array.unsafe_get h_data i) then (
      Array.unsafe_set h_data i
        (Cons {key; data = default; next = Array.unsafe_get h_data i});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h)

  let remove (h : _ t) key =
    let i = key_index h key in
    let h_data = h.data in
    Hash_gen.remove_bucket h i key ~prec:Empty
      (Array.unsafe_get h_data i)
      eq_key

  (* for short bucket list, [find_rec is not called ] *)
  let rec find_rec key (bucketlist : _ bucket) =
    match bucketlist with
    | Empty -> raise Not_found
    | Cons rhs -> if eq_key key rhs.key then rhs.data else find_rec key rhs.next

  let find_exn (h : _ t) key =
    match Array.unsafe_get h.data (key_index h key) with
    | Empty -> raise Not_found
    | Cons rhs -> (
      if eq_key key rhs.key then rhs.data
      else
        match rhs.next with
        | Empty -> raise Not_found
        | Cons rhs -> (
          if eq_key key rhs.key then rhs.data
          else
            match rhs.next with
            | Empty -> raise Not_found
            | Cons rhs ->
              if eq_key key rhs.key then rhs.data else find_rec key rhs.next))

  let find_opt (h : _ t) key =
    Hash_gen.small_bucket_opt eq_key key
      (Array.unsafe_get h.data (key_index h key))

  let find_key_opt (h : _ t) key =
    Hash_gen.small_bucket_key_opt eq_key key
      (Array.unsafe_get h.data (key_index h key))

  let find_default (h : _ t) key default =
    Hash_gen.small_bucket_default eq_key key default
      (Array.unsafe_get h.data (key_index h key))

  let find_all (h : _ t) key =
    let rec find_in_bucket (bucketlist : _ bucket) =
      match bucketlist with
      | Empty -> []
      | Cons rhs ->
        if eq_key key rhs.key then rhs.data :: find_in_bucket rhs.next
        else find_in_bucket rhs.next
    in
    find_in_bucket (Array.unsafe_get h.data (key_index h key))

  let replace h key data =
    let i = key_index h key in
    let h_data = h.data in
    let l = Array.unsafe_get h_data i in
    if Hash_gen.replace_bucket key data l eq_key then (
      Array.unsafe_set h_data i (Cons {key; data; next = l});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h)

  let mem (h : _ t) key =
    Hash_gen.small_bucket_mem
      (Array.unsafe_get h.data (key_index h key))
      eq_key key

  let of_list2 ks vs =
    let len = List.length ks in
    let map = create len in
    List.iter2 (fun k v -> add map k v) ks vs;
    map
end
