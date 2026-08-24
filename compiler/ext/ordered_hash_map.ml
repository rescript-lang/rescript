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

module Make (H : Hashtbl.HashedType) :
  Ordered_hash_map_gen.S with type key = H.t = struct
  type key = H.t
  type 'value t = (key, 'value) Ordered_hash_map_gen.t
  let key_index (h : _ t) key = H.hash key land (Array.length h.data - 1)
  let equal_key = H.equal

  open Ordered_hash_map_gen

  let create = create
  let clear = clear
  let reset = reset

  let iter = iter
  let fold = fold
  let length = length

  let elements = elements
  let choose = choose
  let to_sorted_array = to_sorted_array

  let rec small_bucket_mem key lst =
    match lst with
    | Empty -> false
    | Cons rhs -> (
      equal_key key rhs.key
      ||
      match rhs.next with
      | Empty -> false
      | Cons rhs -> (
        equal_key key rhs.key
        ||
        match rhs.next with
        | Empty -> false
        | Cons rhs -> equal_key key rhs.key || small_bucket_mem key rhs.next))

  let rec small_bucket_rank key lst =
    match lst with
    | Empty -> -1
    | Cons rhs -> (
      if equal_key key rhs.key then rhs.ord
      else
        match rhs.next with
        | Empty -> -1
        | Cons rhs -> (
          if equal_key key rhs.key then rhs.ord
          else
            match rhs.next with
            | Empty -> -1
            | Cons rhs ->
              if equal_key key rhs.key then rhs.ord
              else small_bucket_rank key rhs.next))

  let rec small_bucket_find_value key (lst : (_, _) bucket) =
    match lst with
    | Empty -> raise Not_found
    | Cons rhs -> (
      if equal_key key rhs.key then rhs.data
      else
        match rhs.next with
        | Empty -> raise Not_found
        | Cons rhs -> (
          if equal_key key rhs.key then rhs.data
          else
            match rhs.next with
            | Empty -> raise Not_found
            | Cons rhs ->
              if equal_key key rhs.key then rhs.data
              else small_bucket_find_value key rhs.next))

  let add h key value =
    let i = key_index h key in
    if not (small_bucket_mem key h.data.(i)) then (
      h.data.(i) <- Cons {key; ord = h.size; data = value; next = h.data.(i)};
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h)

  let mem h key =
    small_bucket_mem key (Array.unsafe_get h.data (key_index h key))
  let rank h key =
    small_bucket_rank key (Array.unsafe_get h.data (key_index h key))

  let find_value h key =
    small_bucket_find_value key (Array.unsafe_get h.data (key_index h key))
end
