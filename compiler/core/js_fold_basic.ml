(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

let add_lam_module_ident = Lam_module_ident.Hash_set.add

let create = Lam_module_ident.Hash_set.create

let super = Js_record_iter.super

let count_hard_dependencies hard_dependencies =
  {
    super with
    module_id = (fun _ vid -> add_lam_module_ident hard_dependencies vid);
    expression =
      (fun self x ->
        (match Js_block_runtime.check_additional_id x with
        | Some id ->
          add_lam_module_ident hard_dependencies
            (Lam_module_ident.of_runtime id)
        | _ -> ());
        super.expression self x);
  }

let calculate_hard_dependencies block =
  let hard_dependencies = create 17 in
  let obj = count_hard_dependencies hard_dependencies in
  obj.block obj block;
  hard_dependencies

(*
   Given a set of [variables], count which variables  [lam] will depend on
   Invariant:
   [variables] are parameters which means immutable so that [Call] 
   will not depend [variables]

*)
(* let depends_j (lam : J.expression) (variables : Set_ident.t) =
   let v = ref Set_ident.empty in
   let add id =
    if Set_ident.mem variables id then
      v := Set_ident.add !v id
   in
   ignore @@ (new count_deps add ) # expression lam ;
   !v
*)
