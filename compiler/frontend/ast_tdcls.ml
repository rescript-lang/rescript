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

(**
   [new_tdcls tdcls new_attrs]
   functional update attributes of last declaration *)
let new_tdcls (tdcls : Parsetree.type_declaration list)
    (new_attrs : Parsetree.attributes) : Parsetree.type_declaration list =
  match tdcls with
  | [x] -> [{x with Parsetree.ptype_attributes = new_attrs}]
  | _ ->
    Ext_list.map_last tdcls (fun last x ->
        if last then {x with Parsetree.ptype_attributes = new_attrs} else x)

let handle_tdcls_in_sigi (self : Ast_mapper.mapper)
    (sigi : Parsetree.signature_item) rf
    (tdcls : Parsetree.type_declaration list) : Ast_signature.item =
  match
    Ast_attributes.process_derive_type (Ext_list.last tdcls).ptype_attributes
  with
  | {bs_deriving = Some actions}, new_attrs ->
    let loc = sigi.psig_loc in
    (* remove the processed attr*)
    let new_tdcls_new_attrs =
      List.map (self.type_declaration self) (new_tdcls tdcls new_attrs)
    in
    Ast_signature.fuse_all ~loc
      (Sig.type_ ~loc rf new_tdcls_new_attrs
      :: self.signature self (Ast_derive.gen_signature tdcls actions rf))
  | {bs_deriving = None}, _ ->
    Ast_mapper.default_mapper.signature_item self sigi

let handle_tdcls_in_stru (self : Ast_mapper.mapper)
    (str : Parsetree.structure_item) rf
    (tdcls : Parsetree.type_declaration list) : Ast_structure.item =
  match
    Ast_attributes.process_derive_type (Ext_list.last tdcls).ptype_attributes
  with
  | {bs_deriving = Some actions}, new_attrs ->
    let loc = str.pstr_loc in
    let new_str : Parsetree.structure_item =
      Str.type_ ~loc rf
        (List.map (self.type_declaration self) (new_tdcls tdcls new_attrs))
    in
    Ast_structure.fuse_all ~loc
      (new_str
      :: self.structure self
           (List.map
              (fun action ->
                Ast_derive.gen_structure_signature loc tdcls action rf)
              actions))
  | {bs_deriving = None}, _ -> Ast_mapper.default_mapper.structure_item self str
