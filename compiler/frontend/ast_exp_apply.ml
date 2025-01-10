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

let default_expr_mapper = Bs_ast_mapper.default_mapper.expr

let check_and_discard (args : Ast_compatible.args) =
  Ext_list.map args (fun (label, x) ->
      Bs_syntaxerr.err_if_label x.pexp_loc label;
      x)

type app_pattern = {
  op: string;
  loc: Location.t;
  (* locatoin is the location of whole expression #4451 *)
  args: Parsetree.expression list;
}

let sane_property_name_check loc s =
  if String.contains s '#' then
    Location.raise_errorf ~loc
      "property name (%s) can not contain speical character #" s

(* match fn as *)
let view_as_app (fn : exp) (s : string list) : app_pattern option =
  match fn.pexp_desc with
  | Pexp_apply {funct = {pexp_desc = Pexp_ident {txt = Lident op; _}}; args}
    when Ext_list.has_string s op ->
    Some {op; loc = fn.pexp_loc; args = check_and_discard args}
  | _ -> None

let infix_ops = ["|."; "|.u"; "#="; "##"]

let app_exp_mapper (e : exp) (self : Bs_ast_mapper.mapper) : exp =
  match view_as_app e infix_ops with
  | Some {op = "|." | "|.u"; args = [a_; f_]; loc} -> (
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
    | Pexp_variant (label, None) ->
      {f with pexp_desc = Pexp_variant (label, Some a); pexp_loc = e.pexp_loc}
    | Pexp_construct (ctor, None) ->
      {f with pexp_desc = Pexp_construct (ctor, Some a); pexp_loc = e.pexp_loc}
    | Pexp_apply {funct = fn1; args} ->
      Bs_ast_invariant.warn_discarded_unused_attributes fn1.pexp_attributes;
      {
        pexp_desc = Pexp_apply {funct = fn1; args = (Nolabel, a) :: args};
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
                     | Pexp_construct (ctor, None) ->
                       {
                         fn with
                         pexp_desc = Pexp_construct (ctor, Some bounded_obj_arg);
                       }
                     | Pexp_apply {funct = fn; args} ->
                       Bs_ast_invariant.warn_discarded_unused_attributes
                         fn.pexp_attributes;
                       {
                         Parsetree.pexp_desc =
                           Pexp_apply
                             {
                               funct = fn;
                               args = (Nolabel, bounded_obj_arg) :: args;
                             };
                         pexp_attributes = [];
                         pexp_loc = fn.pexp_loc;
                       }
                     | _ ->
                       Ast_compatible.app1 ~loc:fn.pexp_loc fn bounded_obj_arg));
            pexp_attributes = f.pexp_attributes;
            pexp_loc = f.pexp_loc;
          })
    | _ -> Ast_compatible.app1 ~loc ~attrs:e.pexp_attributes f a)
  | Some {op = "##"; loc; args = [obj; rest]} -> (
    (* - obj##property
       - obj#(method a b )
         we should warn when we discard attributes
         gpr#1063 foo##(bar##baz) we should rewrite (bar##baz)
           first  before pattern match.
           currently the pattern match is written in a top down style.
           Another corner case: f##(g a b [@bs])
    *)
    match rest with
    | {
     pexp_desc =
       ( Pexp_ident {txt = Lident name; _}
       | Pexp_constant (Pconst_string (name, None)) );
     pexp_loc;
    }
    (* f##paint
       TODO: this is not relevant: remove it later
    *) ->
      sane_property_name_check pexp_loc name;
      {e with pexp_desc = Ast_util.js_property loc (self.expr self obj) name}
    | _ -> Location.raise_errorf ~loc "invalid ## syntax")
  (* we can not use [:=] for precedece cases
     like {[i @@ x##length := 3 ]}
     is parsed as {[ (i @@ x##length) := 3]}
     since we allow user to create Js objects in OCaml, it can be of
     ref type
     {[
       let u = object (self)
         val x = ref 3
         method setX x = self##x := 32
         method getX () = !self##x
       end
     ]}
  *)
  | Some {op = "#="; loc; args = [obj; arg]} -> (
    let gen_assignment obj name name_loc =
      sane_property_name_check name_loc name;
      let obj = self.expr self obj in
      let arg = self.expr self arg in
      let fn = Exp.send ~loc obj {txt = name ^ Literals.setter_suffix; loc} in
      Exp.constraint_ ~loc
        (Exp.apply ~loc fn [(Nolabel, arg)])
        (Ast_literal.type_unit ~loc ())
    in
    match obj.pexp_desc with
    | Pexp_send (obj, {txt = name; loc = name_loc}) ->
      gen_assignment obj name name_loc
    | _ -> (
      match view_as_app obj ["##"] with
      | Some
          {
            args =
              [
                obj;
                {
                  pexp_desc =
                    ( Pexp_ident {txt = Lident name}
                    | Pexp_constant (Pconst_string (name, None)) );
                  pexp_loc = name_loc;
                };
              ];
          } ->
        gen_assignment obj name name_loc
      | _ -> Location.raise_errorf ~loc "invalid #= assignment"))
  | Some {op = "|."; loc} ->
    Location.raise_errorf ~loc
      "invalid |. syntax, it can only be used as binary operator"
  | Some {op = "##"; loc} ->
    Location.raise_errorf ~loc
      "Js object ## expect syntax like obj##(paint (a,b)) "
  | Some {op} -> Location.raise_errorf "invalid %s syntax" op
  | None -> default_expr_mapper self e
