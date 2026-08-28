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
let default_typ_mapper = Ast_mapper.default_mapper.typ
(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

(* Turns [(t1, .., tn) => ret] with a method-callback attribute into
   [Js.MethodCallback.arityN<full type>]. *)
let to_method_callback_type loc (mapper : Ast_mapper.mapper) ~arity
    (meth_type : Parsetree.core_type) =
  let meth_type = Ast_mapper.default_mapper.typ mapper meth_type in
  Ast_helper.Typ.constr
    {
      txt = Ldot (Ast_literal.Lid.method_callback, "arity" ^ string_of_int arity);
      loc;
    }
    [meth_type]

let typ_mapper (self : Ast_mapper.mapper) (ty : Parsetree.core_type) =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_arrow {params = _}
  (* let it go without regard label names,
     it will report error later when the label is not empty
  *)
    -> (
    match fst (Ast_attributes.process_attributes_rev ty.ptyp_attributes) with
    | Meth_callback _ -> (
      match ty.ptyp_desc with
      | Ptyp_arrow {params} ->
        to_method_callback_type loc self ~arity:(List.length params) ty
      | _ -> assert false)
    | Nothing -> Ast_mapper.default_mapper.typ self ty)
  | Ptyp_object (methods, closed_flag) ->
    let ( +> ) attr (typ : Parsetree.core_type) =
      {typ with ptyp_attributes = attr :: typ.ptyp_attributes}
    in
    let new_methods =
      Ext_list.fold_right methods [] (fun meth_ acc ->
          match meth_ with
          | Parsetree.Oinherit _ -> meth_ :: acc
          | Parsetree.Otag (label, ptyp_attrs, core_type) ->
            (* Field attributes (including @set, consumed during type
               checking) stay on the field; only method-callback attributes
               move onto the field's type. *)
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev ptyp_attrs with
              | Nothing, attrs -> (attrs, core_type)
              | Meth_callback attr, attrs -> (attrs, attr +> core_type)
            in
            Parsetree.Otag (label, attrs, self.typ self core_type) :: acc)
    in
    {ty with ptyp_desc = Ptyp_object (new_methods, closed_flag)}
  | _ -> default_typ_mapper self ty
