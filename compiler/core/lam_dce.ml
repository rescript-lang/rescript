(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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

let transitive_closure (initial_idents : Ident.t list)
    (ident_freevars : Set_ident.t Hash_ident.t) =
  let visited = Hash_set_ident.create 31 in
  let rec dfs (id : Ident.t) : unit =
    if not (Hash_set_ident.mem visited id || Ext_ident.is_js_or_global id) then (
      Hash_set_ident.add visited id;
      match Hash_ident.find_opt ident_freevars id with
      | None ->
        Ext_fmt.failwithf ~loc:__LOC__ "%s/%d not found" (Ident.name id)
          id.stamp
      | Some e -> Set_ident.iter e dfs)
  in
  Ext_list.iter initial_idents dfs;
  visited

let remove export_idents (rest : Lam_group.t list) : Lam_group.t list =
  let ident_free_vars : _ Hash_ident.t = Hash_ident.create 17 in
  (* calculate initial required idents,
     at the same time, populate dependency set [ident_free_vars]
  *)
  let initial_idents =
    Ext_list.fold_left rest export_idents (fun acc x ->
        match x with
        | Single (kind, id, lam) -> (
          Hash_ident.add ident_free_vars id
            (Lam_free_variables.pass_free_variables lam);
          match kind with
          | Alias | StrictOpt -> acc
          | Strict | Variable -> id :: acc)
        | Recursive bindings ->
          Ext_list.fold_left bindings acc (fun acc (id, lam) ->
              Hash_ident.add ident_free_vars id
                (Lam_free_variables.pass_free_variables lam);
              match lam with
              | Lfunction _ -> acc
              | _ -> id :: acc)
        | Nop lam ->
          if Lam_analysis.no_side_effects lam then acc
          else
            (* its free varaibles here will be defined above *)
            Set_ident.fold (Lam_free_variables.pass_free_variables lam) acc
              (fun x acc -> x :: acc))
  in
  let visited = transitive_closure initial_idents ident_free_vars in
  Ext_list.fold_left rest [] (fun acc x ->
      match x with
      | Single (_, id, _) ->
        if Hash_set_ident.mem visited id then x :: acc else acc
      | Nop _ -> x :: acc
      | Recursive bindings -> (
        let b =
          Ext_list.fold_right bindings [] (fun ((id, _) as v) acc ->
              if Hash_set_ident.mem visited id then v :: acc else acc)
        in
        match b with
        | [] -> acc
        | _ -> Recursive b :: acc))
  |> List.rev
