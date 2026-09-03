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

type outcome = Eval_false | Eval_true | Eval_unknown

let id_is_for_sure_true_in_boolean (tbl : Lam_stats.ident_tbl) id =
  match Hash_ident.find_opt tbl id with
  | Some
      (Normal_optional
         (Lconst (Const_js_false | Const_js_null | Const_js_undefined _))) ->
    Eval_false
  | Some (Constant Const_js_true) -> Eval_true
  | Some (Constant (Const_int i)) -> if i = 0l then Eval_false else Eval_true
  | Some (Constant (Const_js_false | Const_js_null | Const_js_undefined _)) ->
    Eval_false
  | Some
      ( Normal_optional _ | ImmutableBlock _ | MutableBlock _ | Constant _
      | Module _ | FunctionId _ | Exception | Parameter | NA
      | OptionalBlock (_, (Undefined | Null | Null_undefined)) )
  | None ->
    Eval_unknown

let is_const_some (cst : Lambda.structured_constant) : bool =
  match cst with
  | Const_some _ -> true
  | _ -> false

let simplify_alias (meta : Lam_stats.t) (lam : Lambda.t) : Lambda.t =
  let rec simpl (lam : Lambda.t) : Lambda.t =
    match lam with
    (* 7432: prevent optimization in JSX preserve mode *)
    | Lprim
        {
          primitive = Pjs_call {prim_name = "jsx" | "jsxs"} as primitive;
          args = (Lprim {primitive = Pfield (_, _)} as field_arg) :: rest;
          loc;
        }
      when !Js_config.jsx_preserve ->
      Lambda.prim ~primitive ~args:(field_arg :: Ext_list.map rest simpl) loc
    | Lprim {primitive = Pfield (i, info) as primitive; args = [arg]; loc} -> (
      (* ATTENTION:
         Main use case, we should detect inline all immutable block .. *)
      match simpl arg with
      | Lvar v as l ->
        Lam_util.field_flatten_get
          (fun _ ->
            if l == arg then lam else Lambda.prim ~primitive ~args:[l] loc)
          v i info meta.ident_tbl
      | l -> if l == arg then lam else Lambda.prim ~primitive ~args:[l] loc)
    | Lprim
        {
          primitive = (Pval_from_option | Pval_from_option_not_nest) as p;
          args = [(Lvar v as lvar)];
        } as x -> (
      match Hash_ident.find_opt meta.ident_tbl v with
      | Some (OptionalBlock (l, _)) -> l
      | _ -> if p = Pval_from_option_not_nest then lvar else x)
    | Lifthenelse (Lprim {primitive = Pis_not_none; args = [Lvar id]}, l2, l3)
      -> (
      match Hash_ident.find_opt meta.ident_tbl id with
      | Some (Constant c) when is_const_some c -> simpl l2
      | Some (ImmutableBlock _ | MutableBlock _ | Normal_optional _) -> simpl l2
      | Some (OptionalBlock (l, Null)) ->
        Lambda.if_
          (Lambda.not_ Location.none
             (Lambda.prim ~primitive:Pis_null ~args:[l] Location.none))
          (simpl l2) (simpl l3)
      | Some (OptionalBlock (l, Undefined)) ->
        Lambda.if_
          (Lambda.not_ Location.none
             (Lambda.prim ~primitive:Pis_undefined ~args:[l] Location.none))
          (simpl l2) (simpl l3)
      | Some (OptionalBlock (l, Null_undefined)) ->
        Lambda.if_
          (Lambda.not_ Location.none
             (Lambda.prim ~primitive:Pis_null_undefined ~args:[l] Location.none))
          (simpl l2) (simpl l3)
      | Some _ | None -> Lambda_traverse.shallow_map_sharing simpl lam)
    (* could be the code path
       {[ match x with
         | h::hs ->
       ]}
    *)
    | Lifthenelse (l1, l2, l3) -> (
      match l1 with
      | Lvar id -> (
        match id_is_for_sure_true_in_boolean meta.ident_tbl id with
        | Eval_true -> simpl l2
        | Eval_false -> simpl l3
        | Eval_unknown -> Lambda_traverse.shallow_map_sharing simpl lam)
      | _ -> Lambda_traverse.shallow_map_sharing simpl lam)
    (* complicated
           1. inline this function
           2. ...
           exports.Make=
           function(funarg)
       {var $$let=Make(funarg);
         return [0, $$let[5],... $$let[16]]}
    *)
    | Lapply
        {
          ap_func =
            Lprim
              {
                primitive = Pfield (_, Fld_module {name = fld_name});
                args = [Lglobal_module ident];
                _;
              } as l1;
          ap_args = args;
          ap_info;
        } -> (
      match Lam_compile_env.query_external_id_info ident fld_name with
      | {
       persistent_closed_lambda = Some (Lfunction ({params; body} as lfunction));
      }
      (* be more cautious when do cross module inlining *)
        when Ext_list.same_length params args
             && Ext_list.for_all args (fun arg ->
                 match arg with
                 | Lvar p -> (
                   match Hash_ident.find_opt meta.ident_tbl p with
                   | Some v -> v <> Parameter
                   | None -> true)
                 | _ -> true)
             && Lam_analysis.lfunction_can_be_inlined lfunction ->
        simpl (Lam_beta_reduce.propagate_beta_reduce meta params body args)
      | _ ->
        Lambda.apply (simpl l1) (Ext_list.map args simpl) ap_info
          ?ap_transformed_jsx:None)
    (* Function inlining interact with other optimizations...

       - parameter attributes
       - scope issues
       - code bloat
    *)
    | Lapply {ap_func = Lvar v as fn; ap_args; ap_info; ap_transformed_jsx} -> (
      (* Check info for always inlining *)

      (* Ext_log.dwarn __LOC__ "%s/%d" v.name v.stamp;     *)
      let ap_args = Ext_list.map ap_args simpl in
      let[@local] normal () =
        Lambda.apply (simpl fn) ap_args ap_info ~ap_transformed_jsx
      in
      match Hash_ident.find_opt meta.ident_tbl v with
      | Some
          (FunctionId
             {
               lambda =
                 Some
                   ( Lfunction ({params; body; attr = {is_a_functor}} as m),
                     rec_flag );
             })
        when Lam_analysis.lfunction_can_be_inlined m ->
        if Ext_list.same_length ap_args params then
          if is_a_functor (* && (Set_ident.mem v meta.export_idents) && false *)
          then
            (* TODO: check l1 if it is exported,
               if so, maybe not since in that case,
               we are going to have two copy?
            *)
            (* Check: recursive applying may result in non-termination *)
            (* Ext_log.dwarn __LOC__ "beta .. %s/%d" v.name v.stamp ; *)
            simpl
              (Lam_beta_reduce.propagate_beta_reduce meta params body ap_args)
          else if
            (* Lam_analysis.size body < Lam_analysis.small_inline_size *)
            (* ap_inlined = Always_inline || *)
            Lam_analysis.ok_to_inline_fun_when_app m ap_args
          then
            (* let param_map =  *)
            (*   Lam_analysis.free_variables meta.export_idents  *)
            (*     (Lam_analysis.param_map_of_list params) body in *)
            (* let old_count = List.length params in *)
            (* let new_count = Map_ident.cardinal param_map in *)
            let param_map =
              Lam_closure.is_closed_with_map meta.export_idents params body
            in
            let is_export_id = Set_ident.mem meta.export_idents v in
            match (is_export_id, param_map) with
            | false, (_, param_map) | true, (true, param_map) -> (
              match rec_flag with
              | Lam_rec ->
                Lam_beta_reduce.propagate_beta_reduce_with_map meta param_map
                  params body ap_args
              | Lam_self_rec -> normal ()
              | Lam_non_rec ->
                if
                  Ext_list.exists ap_args (fun lam ->
                      Lam_hit.hit_variable v lam)
                  (*avoid nontermination, e.g, `g(g)`*)
                then normal ()
                else
                  simpl
                    (Lam_beta_reduce.propagate_beta_reduce_with_map meta
                       param_map params body ap_args))
            | _ -> normal ()
          else normal ()
        else normal ()
      | Some _ | None -> normal ())
    | Lapply
        {ap_func = Lfunction ({params; body} as lfunction); ap_args = args; _}
      when Ext_list.same_length params args
           && Lam_analysis.lfunction_can_be_inlined lfunction ->
      simpl (Lam_beta_reduce.propagate_beta_reduce meta params body args)
    (* | Lapply{ fn = Lfunction{function_kind =  Tupled;  params; body};  *)
    (*          args = [Lprim {primitive = Pmakeblock _; args; _}]; _} *)
    (*   (\** TODO: keep track of this parameter in ocaml trunk, *)
    (*       can we switch to the tupled backend? *)
    (*   *\) *)
    (*   when  Ext_list.same_length params args -> *)
    (*   simpl (Lam_beta_reduce.propogate_beta_reduce meta params body args) *)
    | Lstringswitch (Lvar s, sw, d)
      when match Hash_ident.find_opt meta.ident_tbl s with
           | Some (Constant _) -> true
           | Some _ | None -> false -> (
      (* The scrutinee is a known constant, so switch on it directly. *)
      match Hash_ident.find_opt meta.ident_tbl s with
      | Some (Constant c) ->
        Lambda.stringswitch (Lambda.const c)
          (Ext_list.map_snd sw simpl)
          (Ext_option.map d simpl)
      | Some _ | None -> Lambda_traverse.shallow_map_sharing simpl lam)
    | _ -> Lambda_traverse.shallow_map_sharing simpl lam
  in
  simpl lam
