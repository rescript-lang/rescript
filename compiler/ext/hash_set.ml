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
module Make (H : Hashtbl.HashedType) : Hash_set_gen.S with type key = H.t =
struct
  type key = H.t
  let eq_key = H.equal
  let key_index (h : _ Hash_set_gen.t) key =
    H.hash key land (Array.length h.data - 1)
  type t = key Hash_set_gen.t

  let create = Hash_set_gen.create
  let clear = Hash_set_gen.clear
  let reset = Hash_set_gen.reset

  (* let copy = Hash_set_gen.copy *)
  let iter = Hash_set_gen.iter
  let fold = Hash_set_gen.fold
  let length = Hash_set_gen.length

  (* let stats = Hash_set_gen.stats *)
  let to_list = Hash_set_gen.to_list

  let remove (h : _ Hash_set_gen.t) key =
    let i = key_index h key in
    let h_data = h.data in
    Hash_set_gen.remove_bucket h i key ~prec:Empty
      (Array.unsafe_get h_data i)
      eq_key

  let add (h : _ Hash_set_gen.t) key =
    let i = key_index h key in
    let h_data = h.data in
    let old_bucket = Array.unsafe_get h_data i in
    if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then (
      Array.unsafe_set h_data i (Cons {key; next = old_bucket});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h)

  let of_array arr =
    let len = Array.length arr in
    let tbl = create len in
    for i = 0 to len - 1 do
      add tbl (Array.unsafe_get arr i)
    done;
    tbl

  let check_add (h : _ Hash_set_gen.t) key : bool =
    let i = key_index h key in
    let h_data = h.data in
    let old_bucket = Array.unsafe_get h_data i in
    if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then (
      Array.unsafe_set h_data i (Cons {key; next = old_bucket});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h;
      true)
    else false

  let mem (h : _ Hash_set_gen.t) key =
    Hash_set_gen.small_bucket_mem eq_key key
      (Array.unsafe_get h.data (key_index h key))
end
