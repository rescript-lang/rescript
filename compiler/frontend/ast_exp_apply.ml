(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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

open Ast_helper

type exp = Parsetree.expression

let rec no_need_bound (exp : exp) =
  match exp.pexp_desc with
  | Pexp_ident {txt = Lident _} -> true
  | Pexp_constraint (e, _) -> no_need_bound e
  | _ -> false

let tuple_obj_id = "__tuple_internal_obj"

let bound (e : exp) (cb : exp -> _) =
  if no_need_bound e then cb e
  else
    let loc = e.pexp_loc in
    Exp.let_ ~loc Nonrecursive
      [Vb.mk ~loc (Pat.var ~loc {txt = tuple_obj_id; loc}) e]
      (cb (Exp.ident ~loc {txt = Lident tuple_obj_id; loc}))

let default_expr_mapper = Ast_mapper.default_mapper.expr

let check_and_discard (args : (Asttypes.arg_label * Parsetree.expression) list)
    =
  Ext_list.map args (fun (label, x) ->
      Bs_syntaxerr.err_if_label x.pexp_loc label;
      x)

type app_pattern = {
  op: string;
  loc: Location.t;
  (* locatoin is the location of whole expression #4451 *)
  args: Parsetree.expression list;
}

(* match fn as *)
let view_as_app (fn : exp) (s : string list) : app_pattern option =
  match fn.pexp_desc with
  | Pexp_apply {funct = {pexp_desc = Pexp_ident {txt = Lident op; _}}; args}
    when Ext_list.has_string s op ->
    Some {op; loc = fn.pexp_loc; args = check_and_discard args}
  | _ -> None

let infix_ops = ["->"]

let app_exp_mapper (e : exp) (self : Ast_mapper.mapper) : exp =
  match view_as_app e infix_ops with
  | Some {op = "->"; args = [a_; f_]; loc} -> (
    (*
        a |. f
        a |. f b c [@bs]  --> f a b c [@bs]
        a |. (g |. b)
        a |. `Variant
        a |. (b |. f c [@bs])
      *)
    let a = self.expr self a_ in
    let f = self.expr self f_ in
    match f.pexp_desc with
    | Pexp_variant (label, {txt = []}) ->
      {
        f with
        pexp_desc = Pexp_variant (label, {txt = [a]; loc = a.pexp_loc});
        pexp_loc = e.pexp_loc;
      }
    | Pexp_construct (ctor, {txt = []}) ->
      {
        f with
        pexp_desc = Pexp_construct (ctor, {txt = [a]; loc = a.pexp_loc});
        pexp_loc = e.pexp_loc;
      }
    | Pexp_apply {funct = fn1; args; partial; transformed_jsx} ->
      Bs_ast_invariant.warn_discarded_unused_attributes fn1.pexp_attributes;
      {
        pexp_desc =
          Pexp_apply
            {funct = fn1; args = (Nolabel, a) :: args; partial; transformed_jsx};
        pexp_loc = e.pexp_loc;
        pexp_attributes = e.pexp_attributes @ f.pexp_attributes;
      }
    | Pexp_tuple xs ->
      bound a (fun bounded_obj_arg ->
          {
            pexp_desc =
              Pexp_tuple
                (Ext_list.map xs (fun fn ->
                     match fn.pexp_desc with
                     | Pexp_construct (ctor, {txt = []}) ->
                       {
                         fn with
                         pexp_desc =
                           Pexp_construct
                             ( ctor,
                               {
                                 txt = [bounded_obj_arg];
                                 loc = bounded_obj_arg.pexp_loc;
                               } );
                       }
                     | Pexp_apply {funct = fn; args; transformed_jsx} ->
                       Bs_ast_invariant.warn_discarded_unused_attributes
                         fn.pexp_attributes;
                       {
                         Parsetree.pexp_desc =
                           Pexp_apply
                             {
                               funct = fn;
                               args = (Nolabel, bounded_obj_arg) :: args;
                               partial = false;
                               transformed_jsx;
                             };
                         pexp_attributes = [];
                         pexp_loc = fn.pexp_loc;
                       }
                     | _ ->
                       Exp.apply ~loc:fn.pexp_loc fn
                         [(Nolabel, bounded_obj_arg)]));
            pexp_attributes = f.pexp_attributes;
            pexp_loc = f.pexp_loc;
          })
    | _ -> Exp.apply ~loc ~attrs:e.pexp_attributes f [(Nolabel, a)])
  | Some {op = "->"; loc} ->
    Location.raise_errorf ~loc
      "Invalid pipe syntax. The pipe symbol (->) can only be used as a binary \
       operator."
  | Some {op} -> Location.raise_errorf "invalid %s syntax" op
  | None -> default_expr_mapper self e
