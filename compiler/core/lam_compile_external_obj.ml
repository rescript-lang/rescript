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

module E = Js_exp_make
module S = Js_stmt_make

(* Note: can potentially be inconsistent, sometimes
   {[
     { x : 3 , y : undefined}
   ]}
   and
   {[
     {x : 3 }
   ]}
   But the default to be undefined  seems reasonable
*)

(* TODO: check stackoverflow *)
let assemble_obj_args (labels : External_arg_spec.obj_params)
    (args : J.expression list) : J.block * J.expression =
  let rec aux (labels : External_arg_spec.obj_params) args :
      (Js_op.property_name * E.t) list * J.expression list * _ =
    match (labels, args) with
    | [], [] -> ([], [], [])
    | ( {obj_arg_label = Obj_label {name = label}; obj_arg_type = Arg_cst cst}
        :: labels,
        args ) ->
      let accs, eff, assign = aux labels args in
      ( (Js_op.Lit label, Lam_compile_const.translate_arg_cst cst) :: accs,
        eff,
        assign )
    (* | {obj_arg_label = EmptyCst _ } :: rest  , args -> assert false  *)
    | {obj_arg_label = Obj_empty} :: labels, arg :: args ->
      (* unit type*)
      let ((accs, eff, assign) as r) = aux labels args in
      if Js_analyzer.no_side_effect_expression arg then r
      else (accs, arg :: eff, assign)
    | ( ({obj_arg_label = Obj_label {name = label}} as arg_kind) :: labels,
        arg :: args ) -> (
      let accs, eff, assign = aux labels args in
      let acc, new_eff =
        Lam_compile_external_call.ocaml_to_js_eff ~arg_label:Arg_label
          ~arg_type:arg_kind.obj_arg_type arg
      in
      match acc with
      | Splice2 _ | Splice0 -> assert false
      | Splice1 x ->
        ((Js_op.Lit label, x) :: accs, Ext_list.append new_eff eff, assign)
      (* evaluation order is undefined *))
    | ( ({obj_arg_label = Obj_optional {name = label}; obj_arg_type} as arg_kind)
        :: labels,
        arg :: args ) ->
      let ((accs, eff, assign) as r) = aux labels args in
      Js_of_lam_option.destruct_optional arg ~for_sure_none:r
        ~for_sure_some:(fun x ->
          let acc, new_eff =
            Lam_compile_external_call.ocaml_to_js_eff ~arg_label:Arg_label
              ~arg_type:obj_arg_type x
          in
          match acc with
          | Splice2 _ | Splice0 -> assert false
          | Splice1 x ->
            ((Js_op.Lit label, x) :: accs, Ext_list.append new_eff eff, assign))
        ~not_sure:(fun _ -> (accs, eff, (arg_kind, arg) :: assign))
    | {obj_arg_label = Obj_empty | Obj_label _ | Obj_optional _} :: _, [] ->
      assert false
    | [], _ :: _ -> assert false
  in
  let map, eff, assignment = aux labels args in
  match assignment with
  | [] ->
    ( [],
      match eff with
      | [] -> E.obj map
      | x :: xs -> E.seq (E.fuse_to_seq x xs) (E.obj map) )
  | _ ->
    let v = Ext_ident.create_tmp () in
    let var_v = E.var v in
    ( S.define_variable ~kind:Variable v
        (match eff with
        | [] -> E.obj map
        | x :: xs -> E.seq (E.fuse_to_seq x xs) (E.obj map))
      :: Ext_list.flat_map assignment
           (fun
             ((xlabel : External_arg_spec.obj_param), (arg : J.expression)) ->
             match xlabel with
             | {
              obj_arg_label =
                Obj_optional {name = label; for_sure_no_nested_option};
             } -> (
               (* Need make sure whether assignment is effectful or not
                  to avoid code duplication
               *)
               match Js_ast_util.named_expression arg with
               | None -> (
                 let acc, new_eff =
                   Lam_compile_external_call.ocaml_to_js_eff
                     ~arg_label:Arg_empty ~arg_type:xlabel.obj_arg_type
                     (if for_sure_no_nested_option then arg
                      else Js_of_lam_option.val_from_option arg)
                 in
                 match acc with
                 | Splice1 v ->
                   [
                     S.if_
                       (Js_of_lam_option.is_not_none arg)
                       [
                         S.exp
                           (E.assign (E.dot var_v label)
                              (match new_eff with
                              | [] -> v
                              | x :: xs -> E.seq (E.fuse_to_seq x xs) v));
                       ];
                   ]
                 | Splice0 | Splice2 _ -> assert false)
               | Some (st, id) -> (
                 (* FIXME: see #2503 *)
                 let arg = E.var id in
                 let acc, new_eff =
                   Lam_compile_external_call.ocaml_to_js_eff
                     ~arg_label:Arg_empty ~arg_type:xlabel.obj_arg_type
                     (if for_sure_no_nested_option then arg
                      else Js_of_lam_option.val_from_option arg)
                 in
                 match acc with
                 | Splice1 v ->
                   [
                     st;
                     S.if_
                       (Js_of_lam_option.is_not_none arg)
                       [
                         S.exp
                           (E.assign (E.dot var_v label)
                              (match new_eff with
                              | [] -> v
                              | x :: xs -> E.seq (E.fuse_to_seq x xs) v));
                       ];
                   ]
                 | Splice0 | Splice2 _ -> assert false))
             | _ -> assert false),
      var_v )
