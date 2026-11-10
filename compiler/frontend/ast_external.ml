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

(* The resolver behind [Primitive.resolve_external], which the type checker
   calls once per declaration: an [@inline] constant, an FFI external with its
   specification, or an intrinsic. An FFI external's errors and warnings are
   reported here, including unused attributes on its resolved form. *)
let resolve (value_desc : Parsetree.value_description) (prim_name : string) :
    Primitive.resolved_external =
  if prim_name = Ast_external_mk.inline_const_prim then
    match Ast_external_mk.inline_const_of_declaration value_desc with
    | Some c ->
      {
        resolved_type = value_desc.pval_type;
        resolved_attributes = [];
        resolved_name = "";
        resolved_kind = Kind_inline_const c;
      }
    | None ->
      Location.raise_errorf ~loc:value_desc.pval_loc
        "\"%s\" is reserved for %@inline constants" prim_name
  else if Ast_attributes.is_ffi_external value_desc.pval_attributes prim_name
  then (
    let {Ast_external_process.pval_type; spec; pval_attributes} =
      Ast_external_process.resolve value_desc.pval_loc value_desc.pval_type
        value_desc.pval_attributes prim_name
    in
    Bs_ast_invariant.emit_external_warnings_on_resolved_external value_desc
      ~pval_type ~pval_attributes;
    {
      resolved_type = pval_type;
      resolved_attributes = pval_attributes;
      resolved_name = prim_name;
      resolved_kind = Kind_external spec;
    })
  else
    {
      resolved_type = value_desc.pval_type;
      resolved_attributes = value_desc.pval_attributes;
      resolved_name = prim_name;
      resolved_kind = Kind_intrinsic;
    }
