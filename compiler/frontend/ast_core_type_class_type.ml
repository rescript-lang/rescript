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

let process_getter_setter ~not_getter_setter
    ~(get : Parsetree.core_type -> _ -> Parsetree.attributes -> _) ~set loc name
    (attrs : Ast_attributes.t) (ty : Parsetree.core_type) (acc : _ list) =
  match Ast_attributes.process_method_attributes_rev attrs with
  | {get = None; set = None}, _ -> not_getter_setter ty :: acc
  | st, pctf_attributes ->
    let get_acc =
      match st.set with
      | Some `No_get -> acc
      | None | Some `Get ->
        let lift txt = Typ.constr ~loc {txt; loc} [ty] in
        let null, undefined =
          match st with
          | {get = Some (null, undefined)} -> (null, undefined)
          | {get = None} -> (false, false)
        in
        let ty =
          match (null, undefined) with
          | false, false -> ty
          | true, false -> lift Ast_literal.Lid.js_null
          | false, true -> lift Ast_literal.Lid.js_undefined
          | true, true -> lift Ast_literal.Lid.js_null_undefined
        in
        get ty name pctf_attributes :: acc
    in
    if st.set = None then get_acc
    else
      set ty
        ({name with txt = name.Asttypes.txt ^ Literals.setter_suffix}
          : _ Asttypes.loc)
        pctf_attributes
      :: get_acc

let default_typ_mapper = Bs_ast_mapper.default_mapper.typ
(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

let typ_mapper (self : Bs_ast_mapper.mapper) (ty : Parsetree.core_type) =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_arrow {lbl = label; arg = args; ret = body}
  (* let it go without regard label names,
     it will report error later when the label is not empty
  *)
    -> (
    match fst (Ast_attributes.process_attributes_rev ty.ptyp_attributes) with
    | Meth_callback _ ->
      Ast_typ_uncurry.to_method_callback_type loc self label args body
    | Method _ ->
      (* Treat @meth as making the type uncurried, for backwards compatibility *)
      Ast_typ_uncurry.to_uncurry_type loc self label args body
    | Nothing -> Bs_ast_mapper.default_mapper.typ self ty)
  | Ptyp_object (methods, closed_flag) ->
    let ( +> ) attr (typ : Parsetree.core_type) =
      {typ with ptyp_attributes = attr :: typ.ptyp_attributes}
    in
    let new_methods =
      Ext_list.fold_right methods [] (fun meth_ acc ->
          match meth_ with
          | Parsetree.Oinherit _ -> meth_ :: acc
          | Parsetree.Otag (label, ptyp_attrs, core_type) ->
            let get ty name attrs =
              let attrs, core_type =
                match Ast_attributes.process_attributes_rev attrs with
                | Nothing, attrs -> (attrs, ty) (* #1678 *)
                | Method _, _ ->
                  Location.raise_errorf ~loc "%@get/set conflicts with %@meth"
                | Meth_callback attr, attrs -> (attrs, attr +> ty)
              in
              Ast_compatible.object_field name attrs (self.typ self core_type)
            in
            let set ty name attrs =
              let attrs, core_type =
                match Ast_attributes.process_attributes_rev attrs with
                | Nothing, attrs -> (attrs, ty)
                | Method _, _ ->
                  Location.raise_errorf ~loc "%@get/set conflicts with %@meth"
                | Meth_callback attr, attrs -> (attrs, attr +> ty)
              in
              Ast_compatible.object_field name attrs
                (Ast_typ_uncurry.to_uncurry_type loc self Nolabel core_type
                   (Ast_literal.type_unit ~loc ()))
            in
            let not_getter_setter ty =
              let attrs, core_type =
                match Ast_attributes.process_attributes_rev ptyp_attrs with
                | Nothing, attrs -> (attrs, ty)
                | Method attr, attrs -> (attrs, attr +> ty)
                | Meth_callback attr, attrs -> (attrs, attr +> ty)
              in
              Ast_compatible.object_field label attrs (self.typ self core_type)
            in
            process_getter_setter ~not_getter_setter ~get ~set loc label
              ptyp_attrs core_type acc)
    in
    {ty with ptyp_desc = Ptyp_object (new_methods, closed_flag)}
  | _ -> default_typ_mapper self ty
