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

(* let pp = Format.fprintf  *)
(* we should exclude meaninglist names and do the convert as well *)

(* let meaningless_names  = ["*opt*"; "param";] *)

let single_na = Js_cmj_format.single_na

let values_of_export (meta : Lam_stats.t) (export_map : Lam.t Map_ident.t) :
    Js_cmj_format.cmj_value Map_string.t =
  Ext_list.fold_left meta.exports Map_string.empty (fun acc x ->
      let arity : Js_cmj_format.arity =
        match Hash_ident.find_opt meta.ident_tbl x with
        | Some (FunctionId {arity; _}) -> Single arity
        | Some (ImmutableBlock elems) ->
          (* FIXME: field name for dumping*)
          Submodule
            (Ext_array.map elems (fun x ->
                 match x with
                 | NA -> Lam_arity.na
                 | SimpleForm lam -> Lam_arity_analysis.get_arity meta lam))
        | Some _ | None -> (
          match Map_ident.find_opt export_map x with
          | Some (Lprim {primitive = Pmakeblock (_, _, Immutable); args}) ->
            Submodule
              (Ext_array.of_list_map args (fun lam ->
                   Lam_arity_analysis.get_arity meta lam))
          | Some _ | None -> single_na)
      in
      let persistent_closed_lambda =
        let optlam = Map_ident.find_opt export_map x in
        match optlam with
        | Some
            (Lconst
               ( Const_js_null | Const_js_undefined _ | Const_js_true
               | Const_js_false ))
        | None ->
          optlam
        | Some lambda ->
          if not !Js_config.cross_module_inline then None
          else if
            Lam_analysis.safe_to_inline lambda
            (* when inlning a non function, we have to be very careful,
               only truly immutable values can be inlined
            *)
          then
            match lambda with
            | Lfunction {attr = {inline = Always_inline}}
            (* FIXME: is_closed lambda is too restrictive
               It precludes ues cases
               - inline forEach but not forEachU
            *)
            | Lfunction {attr = {is_a_functor = true}} ->
              if Lam_closure.is_closed lambda (* TODO: seriealize more*) then
                optlam
              else None
            | _ ->
              let lam_size = Lam_analysis.size lambda in
              (* TODO:
                 1. global need re-assocate when do the beta reduction
                 2. [lambda_exports] is not precise
              *)
              let free_variables =
                Lam_closure.free_variables Set_ident.empty Map_ident.empty
                  lambda
              in
              if
                lam_size < Lam_analysis.small_inline_size
                && Map_ident.is_empty free_variables
              then (
                Ext_log.dwarn ~__POS__ "%s recorded for inlining @." x.name;
                optlam)
              else None
          else None
      in
      match (arity, persistent_closed_lambda) with
      | Single Arity_na, (None | Some (Lconst Const_module_alias)) -> acc
      | Submodule [||], None -> acc
      | _ ->
        let cmj_value : Js_cmj_format.cmj_value =
          {arity; persistent_closed_lambda}
        in
        Map_string.add acc x.name cmj_value)

(* ATTENTION: all runtime modules, if it is not hard required,
   it should be okay to not reference it
*)
let get_dependent_module_effect (maybe_pure : string option)
    (external_ids : Lam_module_ident.t list) =
  if maybe_pure = None then
    let non_pure_module =
      Ext_list.find_first_not external_ids Lam_compile_env.is_pure_module
    in
    Ext_option.map non_pure_module (fun x -> Lam_module_ident.name x)
  else maybe_pure

(* Note that
   [lambda_exports] is
   lambda expression to be exported
   for the js backend, we compile to js
   for the inliner, we try to serialize it --
   relies on other optimizations to make this happen
   {[
     exports.Make = function () {.....}
   ]}
   TODO: check that we don't do this in browser environment
*)
let export_to_cmj (meta : Lam_stats.t) effect_ export_map case : Js_cmj_format.t
    =
  let values = values_of_export meta export_map in

  Js_cmj_format.make ~values ~effect_
    ~package_spec:(Js_packages_state.get_packages_info ())
    ~case
(* FIXME: make sure [-o] would not change its case
   add test for ns/non-ns
*)
