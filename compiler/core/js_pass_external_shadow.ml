(* Copyright (C) 2025 - Authors of ReScript
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

module E = Js_exp_make

module StringSet = Set.Make (String)

let global_this = E.js_global "globalThis"

(* Only rewrite for lexical bindings (let-style). *)
let is_lexical_binding_kind (property : J.property) =
  match property with
  | Strict | StrictOpt | Alias -> true
  | Variable -> false

(* Skip JS globals/keywords and compiler-reserved JS ids. *)
let should_rewrite_binding (ident : Ident.t) =
  (not (Ext_ident.is_js ident))
  && (not (Js_reserved_map.is_js_keyword ident.name))
  && not (Js_reserved_map.is_js_global ident.name)

let rewrite_shadowed_global_in_expr ~(name : string) (expr : J.expression) :
    J.expression =
  let super = Js_record_map.super in
  let self =
    {
      super with
      expression =
        (fun self expr ->
          match expr.expression_desc with
          | Var (Id id) when Ext_ident.is_js id && String.equal id.name name ->
            E.dot global_this name
          | _ -> super.expression self expr);
    }
  in
  self.expression self expr

let program (js : J.program) : J.program =
  let shadowed_globals =
    Ext_list.fold_left js.block StringSet.empty (fun acc (st : J.statement) ->
        match st.statement_desc with
        | Variable {ident; property}
          when is_lexical_binding_kind property && should_rewrite_binding ident
          ->
          StringSet.add ident.name acc
        | _ -> acc)
  in
  let super = Js_record_map.super in
  let self =
    {
      super with
      expression =
        (fun self expr ->
          match expr.expression_desc with
          | Static_index (obj, field, pos) ->
            let obj = self.expression self obj in
            let obj =
              match obj.expression_desc with
              | Var (Id id)
                when Ext_ident.is_js id
                     && StringSet.mem id.name shadowed_globals ->
                E.dot global_this id.name
              | _ -> obj
            in
            {expr with expression_desc = Static_index (obj, field, pos)}
          | _ -> super.expression self expr);
      variable_declaration =
        (fun self (vd : J.variable_declaration) ->
          match vd with
          | {ident; value = Some expr; property}
            when is_lexical_binding_kind property
                 && should_rewrite_binding ident ->
            let expr = rewrite_shadowed_global_in_expr ~name:ident.name expr in
            super.variable_declaration self {vd with value = Some expr}
          | _ -> super.variable_declaration self vd);
    }
  in
  self.program self js
