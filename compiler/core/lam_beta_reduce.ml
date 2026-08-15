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

(*
     A naive beta reduce would break the invariants of the optmization.


     The sane but slowest  way:
       when we do a beta reduction, we need rename all variables inlcuding
       let-bound ones

     A conservative one:
       - for internal one
         rename params and let bound variables
       - for external one (seriaized)
         if it's enclosed environment should be good enough
         so far, we only inline enclosed lambdas
     TODO: rename

    Optimizations:
    {[
      (fun x y -> ...     ) 100 3
    ]}
    we can bound [x] to [100] in a single step
*)
let propagate_beta_reduce (meta : Lam_stats.t) (params : Ident.t list)
    (body : Lam.t) (args : Lam.t list) =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with
  | Some x -> x
  | None ->
    let rest_bindings, rev_new_params =
      Ext_list.fold_left2 params args ([], [])
        (fun old_param arg (rest_bindings, acc) ->
          match arg with
          | Lconst _ | Lvar _ -> (rest_bindings, arg :: acc)
          | _ ->
            let p = Ident.rename old_param in
            ((p, arg) :: rest_bindings, Lam.var p :: acc))
    in
    let new_body =
      Lam_bounded_vars.rewrite
        (Hash_ident.of_list2 (List.rev params) rev_new_params)
        body
    in
    Ext_list.fold_right rest_bindings new_body (fun (param, arg) l ->
        (match arg with
        | Lprim {primitive = Pmakeblock (_, _, Immutable); args; _} ->
          Hash_ident.replace meta.ident_tbl param
            (Lam_util.kind_of_lambda_block args)
        | Lprim {primitive = Psome | Psome_not_nest; args = [v]; _} ->
          Hash_ident.replace meta.ident_tbl param (Normal_optional v)
        | _ -> ());
        Lam_util.refine_let ~kind:Strict param arg l)

let propagate_beta_reduce_with_map (meta : Lam_stats.t)
    (map : Lam_var_stats.stats Map_ident.t) params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with
  | Some x -> x
  | None ->
    let rest_bindings, rev_new_params =
      Ext_list.fold_left2 params args ([], [])
        (fun old_param arg (rest_bindings, acc) ->
          match arg with
          | Lconst _ | Lvar _ -> (rest_bindings, arg :: acc)
          | Lglobal_module _ ->
            let p = Ident.rename old_param in
            ((p, arg) :: rest_bindings, Lam.var p :: acc)
          | _ ->
            if Lam_analysis.no_side_effects arg then
              match Map_ident.find_exn map old_param with
              | stat ->
                if Lam_var_stats.top_and_used_zero_or_one stat then
                  (rest_bindings, arg :: acc)
                else
                  let p = Ident.rename old_param in
                  ((p, arg) :: rest_bindings, Lam.var p :: acc)
            else
              let p = Ident.rename old_param in
              ((p, arg) :: rest_bindings, Lam.var p :: acc))
    in
    let new_body =
      Lam_bounded_vars.rewrite
        (Hash_ident.of_list2 (List.rev params) rev_new_params)
        body
    in
    Ext_list.fold_right rest_bindings new_body (fun (param, (arg : Lam.t)) l ->
        (match arg with
        | Lprim {primitive = Pmakeblock (_, _, Immutable); args} ->
          Hash_ident.replace meta.ident_tbl param
            (Lam_util.kind_of_lambda_block args)
        | Lprim {primitive = Psome | Psome_not_nest; args = [v]} ->
          Hash_ident.replace meta.ident_tbl param (Normal_optional v)
        | _ -> ());
        Lam_util.refine_let ~kind:Strict param arg l)

let no_names_beta_reduce params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with
  | Some x -> x
  | None ->
    Ext_list.fold_left2 params args body (fun param arg l ->
        Lam_util.refine_let ~kind:Strict param arg l)
