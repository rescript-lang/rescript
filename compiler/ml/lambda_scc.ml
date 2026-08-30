(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

open Lambda

type bindings = (Ident.t * lambda) list

(* [p] may have side effects (masking). Returning true stops the walk. *)
let exists_var (p : Ident.t -> bool) (l : lambda) : bool =
  let rec hit_opt = function
    | None -> false
    | Some a -> hit a
  and hit_list_snd : 'a. ('a * lambda) list -> bool =
   fun x -> Ext_list.exists_snd x hit
  and hit_list xs = Ext_list.exists xs hit
  and hit (l : lambda) =
    match l with
    | Lvar id -> p id
    | Lassign (id, e) -> p id || hit e
    | Lstaticcatch (e1, _, e2)
    | Ltrywith (e1, _, e2)
    | Lsequence (e1, e2)
    | Lwhile (e1, e2)
    | Lfor_of (_, e1, e2)
    | Lfor_await_of (_, e1, e2) ->
      hit e1 || hit e2
    | Lfunction {body} -> hit body
    | Llet (_, _, _, arg, body) -> hit arg || hit body
    | Lletrec (decl, body) -> hit body || hit_list_snd decl
    | Lfor (_, e1, e2, _, e3) | Lifthenelse (e1, e2, e3) ->
      hit e1 || hit e2 || hit e3
    | Lconst _ | Lbreak | Lcontinue -> false
    | Lapply {ap_func; ap_args} -> hit ap_func || hit_list ap_args
    | Lprim (_, args, _) | Lstaticraise (_, args) -> hit_list args
    | Lswitch (arg, sw, _) ->
      hit arg || hit_list_snd sw.sw_consts || hit_list_snd sw.sw_blocks
      || hit_opt sw.sw_failaction
    | Lstringswitch (arg, cases, default, _) ->
      hit arg || hit_list_snd cases || hit_opt default
  in
  hit l

let preprocess_deps (groups : bindings) : _ * Ident.t array * Vec_int.t array =
  let len = List.length groups in
  let domain : _ Ordered_hash_map_local_ident.t =
    Ordered_hash_map_local_ident.create len
  in
  let mask = Hash_set_ident_mask.create len in
  Ext_list.iter groups (fun (x, lam) ->
      Ordered_hash_map_local_ident.add domain x lam;
      Hash_set_ident_mask.add_unmask mask x);
  let int_mapping = Ordered_hash_map_local_ident.to_sorted_array domain in
  let node_vec =
    Array.init (Array.length int_mapping) (fun _ -> Vec_int.empty ())
  in
  Ordered_hash_map_local_ident.iter domain (fun _id lam key_index ->
      let base_key = node_vec.(key_index) in
      ignore (exists_var (Hash_set_ident_mask.mask_and_check_all_hit mask) lam);
      Hash_set_ident_mask.iter_and_unmask mask (fun ident hit ->
          if hit then
            let key = Ordered_hash_map_local_ident.rank domain ident in
            Vec_int.push base_key key));
  (domain, int_mapping, node_vec)

let bind_rec (groups : bindings) (body : lambda) : lambda =
  match groups with
  | [(id, bind)] ->
    if exists_var (Ident.same id) bind then Lletrec (groups, body)
    else Llet (Strict, Pgenval, id, bind, body)
  | _ ->
    let domain, int_mapping, node_vec = preprocess_deps groups in
    let clusters = Ext_scc.graph node_vec in
    if Int_vec_vec.length clusters <= 1 then Lletrec (groups, body)
    else
      Int_vec_vec.fold_right
        (fun (v : Vec_int.t) acc ->
          let bindings =
            Vec_int.map_into_list
              (fun i ->
                let id = int_mapping.(i) in
                let lam = Ordered_hash_map_local_ident.find_value domain id in
                (id, lam))
              v
          in
          match bindings with
          | [(id, lam)] ->
            let base_key = Ordered_hash_map_local_ident.rank domain id in
            if Int_vec_util.mem base_key node_vec.(base_key) then
              Lletrec (bindings, acc)
            else Llet (Strict, Pgenval, id, lam, acc)
          | _ -> Lletrec (bindings, acc))
        clusters body
