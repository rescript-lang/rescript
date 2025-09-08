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

(** A speicalized datastructure for scc algorithm *)

type ident = Ident.t

type bucket = Empty | Cons of {ident: ident; mutable mask: bool; rest: bucket}

type t = {
  mutable size: int;
  mutable data: bucket array;
  mutable mask_size: int; (* mark how many idents are marked *)
}

let key_index_by_ident (h : t) (key : Ident.t) =
  Bs_hash_stubs.hash_string_int key.name key.stamp land (Array.length h.data - 1)

let create initial_size =
  let s = Ext_util.power_2_above 8 initial_size in
  {size = 0; data = Array.make s Empty; mask_size = 0}

let iter_and_unmask h f =
  let rec iter_bucket buckets =
    match buckets with
    | Empty -> ()
    | Cons k ->
      let k_mask = k.mask in
      f k.ident k_mask;
      if k_mask then (
        k.mask <- false;
        (* we can set [h.mask_size] to zero,
           however, it would result inconsistent state
           once [f] throw
        *)
        h.mask_size <- h.mask_size - 1);
      iter_bucket k.rest
  in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    iter_bucket (Array.unsafe_get d i)
  done

let rec small_bucket_mem key lst =
  match lst with
  | Empty -> false
  | Cons rst -> (
    Ext_ident.equal key rst.ident
    ||
    match rst.rest with
    | Empty -> false
    | Cons rst -> (
      Ext_ident.equal key rst.ident
      ||
      match rst.rest with
      | Empty -> false
      | Cons rst ->
        Ext_ident.equal key rst.ident || small_bucket_mem key rst.rest))

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then (
    let ndata = Array.make nsize Empty in
    h.data <- ndata;
    (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
      | Empty -> ()
      | Cons {ident = key; mask; rest} ->
        let nidx = indexfun h key in
        Array.unsafe_set ndata nidx
          (Cons {ident = key; mask; rest = Array.unsafe_get ndata nidx});
        insert_bucket rest
    in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done)

let add_unmask (h : t) (key : Ident.t) =
  let i = key_index_by_ident h key in
  let h_data = h.data in
  let old_bucket = Array.unsafe_get h_data i in
  if not (small_bucket_mem key old_bucket) then (
    Array.unsafe_set h_data i
      (Cons {ident = key; mask = false; rest = old_bucket});
    h.size <- h.size + 1;
    if h.size > Array.length h_data lsl 1 then resize key_index_by_ident h)

let rec small_bucket_mask key lst =
  match lst with
  | Empty -> false
  | Cons rst -> (
    if Ext_ident.equal key rst.ident then
      if rst.mask then false
      else (
        rst.mask <- true;
        true)
    else
      match rst.rest with
      | Empty -> false
      | Cons rst -> (
        if Ext_ident.equal key rst.ident then
          if rst.mask then false
          else (
            rst.mask <- true;
            true)
        else
          match rst.rest with
          | Empty -> false
          | Cons rst ->
            if Ext_ident.equal key rst.ident then
              if rst.mask then false
              else (
                rst.mask <- true;
                true)
            else small_bucket_mask key rst.rest))

let mask_and_check_all_hit (h : t) (key : Ident.t) =
  if small_bucket_mask key (Array.unsafe_get h.data (key_index_by_ident h key))
  then h.mask_size <- h.mask_size + 1;
  h.size = h.mask_size
