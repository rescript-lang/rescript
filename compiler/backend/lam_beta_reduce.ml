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
let param_table params new_params =
  let tbl = Hash_ident.create (List.length params) in
  List.iter2 (Hash_ident.add tbl) params new_params;
  tbl

(* Whether [body] reads [param] only before it runs code with side effects.
   A read in code that may run later, such as a function or a loop, does not
   count as before. *)
let read_before_effects param (body : Lambda.t) =
  let exception Read_after_effects in
  let effects = ref false in
  let rec walk (lam : Lambda.t) =
    match lam with
    | Lvar v ->
      if !effects && Ident.same v param then raise_notrace Read_after_effects
    | Lconst _ | Lglobal_module _ -> ()
    | Lprim {args} ->
      List.iter walk args;
      if not (Lam_analysis.no_side_effects lam) then effects := true
    | Lapply {ap_func; ap_args} ->
      walk ap_func;
      List.iter walk ap_args;
      effects := true
    | Llet (_, _, arg, body) ->
      walk arg;
      walk body
    | Lsequence (a, b) ->
      walk a;
      walk b
    | Lifthenelse (cond, then_, else_) ->
      walk cond;
      let before = !effects in
      walk then_;
      let after_then = !effects in
      effects := before;
      walk else_;
      effects := !effects || after_then
    | Lfunction _ | Lletrec _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
    | Lstaticcatch _ | Ltrywith _ | Ltryfinally _ | Lwhile _ | Lfor _
    | Lfor_of _ | Lfor_await_of _ | Lassign _ | Lbreak | Lcontinue ->
      if Lam_hit.hit_variable param lam then raise_notrace Read_after_effects;
      if not (Lam_analysis.no_side_effects lam) then effects := true
  in
  match walk body with
  | () -> true
  | exception Read_after_effects -> false

(* The arguments of a call run left to right, then the body. Putting the
   argument for [param] where [body] reads it instead moves it after the
   arguments to its right and the code of [body] before the read. That changes
   nothing when it reads no state that may change. One that only reads may
   still move when no argument to its right has side effects
   ([effects_after]) and [body] reads it before any. *)
let can_move ~assignable ~effects_after param body (arg : Lambda.t) =
  Lam_analysis.delay_safe ~assignable arg
  || Lam_analysis.no_side_effects arg
     && (not effects_after)
     && read_before_effects param body

(* For each argument, whether an argument to its right has side effects *)
let effects_after args =
  snd
    (List.fold_right
       (fun arg (effects, acc) ->
         (effects || not (Lam_analysis.no_side_effects arg), effects :: acc))
       args (false, []))

let propagate_beta_reduce (meta : Lam_stats.t) (params : Ident.t list)
    (body : Lambda.t) (args : Lambda.t list) =
  let assignable = meta.assignable in
  match
    Lam_beta_reduce_util.simple_beta_reduce ~assignable params body args
  with
  | Some x -> x
  | None ->
    let rest_bindings, rev_new_params =
      Ext_list.fold_left2 params
        (List.combine args (effects_after args))
        ([], [])
        (fun old_param (arg, effects_after) (rest_bindings, acc) ->
          match arg with
          | Lconst _ -> (rest_bindings, arg :: acc)
          | Lvar _ when can_move ~assignable ~effects_after old_param body arg
            ->
            (rest_bindings, arg :: acc)
          | _ ->
            let p = Ident.rename old_param in
            ((p, arg) :: rest_bindings, Lambda.var p :: acc))
    in
    let new_body =
      Lam_bounded_vars.rewrite
        (param_table (List.rev params) rev_new_params)
        body
    in
    (* [rest_bindings] is in reverse parameter order; folding left makes the
       first parameter's binding outermost, so arguments evaluate in call
       order. *)
    Ext_list.fold_left rest_bindings new_body (fun l (param, arg) ->
        (match arg with
        | Lprim {primitive = Pmakeblock info; args; _}
          when Lambda.is_immutable_block info ->
          Hash_ident.replace meta.ident_tbl param
            (Lam_util.kind_of_lambda_block args)
        | Lprim {primitive = Psome | Psome_not_nest; args = [v]; _} ->
          Hash_ident.replace meta.ident_tbl param (Normal_optional v)
        | _ -> ());
        Lam_util.refine_let ~assignable ~kind:Strict param arg l)

let propagate_beta_reduce_with_map (meta : Lam_stats.t)
    (map : Lam_var_stats.stats Map_ident.t) params body args =
  let assignable = meta.assignable in
  match
    Lam_beta_reduce_util.simple_beta_reduce ~assignable params body args
  with
  | Some x -> x
  | None ->
    let rest_bindings, rev_new_params =
      Ext_list.fold_left2 params
        (List.combine args (effects_after args))
        ([], [])
        (fun old_param (arg, effects_after) (rest_bindings, acc) ->
          let bind () =
            let p = Ident.rename old_param in
            ((p, arg) :: rest_bindings, Lambda.var p :: acc)
          in
          match arg with
          | Lconst _ -> (rest_bindings, arg :: acc)
          | Lvar _ ->
            if can_move ~assignable ~effects_after old_param body arg then
              (rest_bindings, arg :: acc)
            else bind ()
          | Lglobal_module _ -> bind ()
          | _ ->
            if
              Lam_analysis.no_side_effects arg
              && Lam_var_stats.top_and_used_zero_or_one
                   (Map_ident.find old_param map)
              && can_move ~assignable ~effects_after old_param body arg
            then (rest_bindings, arg :: acc)
            else bind ())
    in
    let new_body =
      Lam_bounded_vars.rewrite
        (param_table (List.rev params) rev_new_params)
        body
    in
    (* See above: fold left so arguments evaluate in call order. *)
    Ext_list.fold_left rest_bindings new_body
      (fun l (param, (arg : Lambda.t)) ->
        (match arg with
        | Lprim {primitive = Pmakeblock info; args}
          when Lambda.is_immutable_block info ->
          Hash_ident.replace meta.ident_tbl param
            (Lam_util.kind_of_lambda_block args)
        | Lprim {primitive = Psome | Psome_not_nest; args = [v]} ->
          Hash_ident.replace meta.ident_tbl param (Normal_optional v)
        | _ -> ());
        Lam_util.refine_let ~assignable ~kind:Strict param arg l)

let no_names_beta_reduce ~assignable params body args =
  match
    Lam_beta_reduce_util.simple_beta_reduce ~assignable params body args
  with
  | Some x -> x
  | None ->
    (* Fold right, so that the first parameter's binding is outermost and the
       arguments evaluate in call order *)
    Ext_list.fold_right2 params args body (fun param arg l ->
        Lam_util.refine_let ~assignable ~kind:Strict param arg l)
