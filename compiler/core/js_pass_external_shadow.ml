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

module String_set = Set.Make (String)

let global_this = E.js_global "globalThis"

(* A reference to a JS global [x] is printed as [x]. If a binding printed as
   [x] is in scope there, that name refers to the binding instead, so the
   reference has to go through [globalThis.x]. *)

(* The name a binding is printed with. Bindings named like JS keywords or
   reserved globals are printed with a [$$] prefix, so they shadow nothing. *)
let add_binding (ident : Ident.t) names =
  if Ext_ident.is_js ident then names
  else String_set.add (Ext_ident.convert ident.name) names

(* A JS [let], [const] or function declaration is in scope in its whole block,
   including before the declaration. *)
let add_declarations (block : J.block) names =
  Ext_list.fold_left block names (fun acc (st : J.statement) ->
      match st.statement_desc with
      | Variable {ident} -> add_binding ident acc
      | _ -> acc)

(* The cases of a switch are printed without braces, so they share one scope. *)
let add_switch_declarations (clauses : (_ * J.case_clause) list)
    (default : J.block option) names =
  let names =
    Ext_list.fold_left clauses names (fun acc (_, c) ->
        add_declarations c.switch_body acc)
  in
  match default with
  | Some block -> add_declarations block names
  | None -> names

(* Statically imported modules are toplevel bindings of the generated module
   ([import * as M from ...] or [let M = require(...)]). *)
let imported_modules (js : J.program) =
  let names = ref String_set.empty in
  let super = Js_record_iter.super in
  let self =
    {
      super with
      module_id =
        (fun _ (m : J.module_id) ->
          if not m.dynamic_import then names := add_binding m.id !names);
    }
  in
  self.program self js;
  !names

let program (js : J.program) : J.program =
  (* printed names of the bindings in scope at the current point *)
  let in_scope = ref (imported_modules js) in
  let with_bindings add f =
    let outer = !in_scope in
    in_scope := add outer;
    let result = f () in
    in_scope := outer;
    result
  in
  let super = Js_record_map.super in
  let self =
    {
      super with
      block =
        (fun self block ->
          with_bindings (add_declarations block) (fun () ->
              super.block self block));
      expression =
        (fun self expr ->
          match expr.expression_desc with
          | Var (Id id)
            when Ext_ident.is_js id && String_set.mem id.name !in_scope ->
            E.dot global_this id.name
          | Fun {params} ->
            with_bindings
              (fun names ->
                Ext_list.fold_left params names (fun acc p -> add_binding p acc))
              (fun () -> super.expression self expr)
          | _ -> super.expression self expr);
      statement =
        (fun self (st : J.statement) ->
          match st.statement_desc with
          | ForRange (_, _, _, ident, _, _)
          | ForOf (_, ident, _, _)
          | ForAwaitOf (_, ident, _, _) ->
            with_bindings (add_binding ident) (fun () ->
                super.statement self st)
          (* The cases of a switch are printed without braces, so they share
             one scope. *)
          | Int_switch (_, clauses, default) ->
            with_bindings (add_switch_declarations clauses default) (fun () ->
                super.statement self st)
          | String_switch (_, clauses, default) ->
            with_bindings (add_switch_declarations clauses default) (fun () ->
                super.statement self st)
          (* The catch parameter is only in scope in the handler. *)
          | Try (body, catch, finally) ->
            let body = self.block self body in
            let catch =
              match catch with
              | None -> None
              | Some (ident, handler) ->
                Some
                  ( ident,
                    with_bindings (add_binding ident) (fun () ->
                        self.block self handler) )
            in
            let finally =
              match finally with
              | None -> None
              | Some block -> Some (self.block self block)
            in
            {st with statement_desc = Try (body, catch, finally)}
          | _ -> super.statement self st);
    }
  in
  self.program self js
