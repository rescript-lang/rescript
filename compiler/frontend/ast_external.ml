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
  else if
    Ast_attributes.rs_externals value_desc.pval_attributes value_desc.pval_prim
  then
    (* [handle_external_in_sig] and [handle_external_in_stru] resolved this
       declaration already and reported its warnings; resolve it again for
       the specification without repeating them. *)
    let {Ast_external_process.pval_type; spec; pval_attributes} =
      Warnings.without_warnings (fun () ->
          Ast_external_process.resolve value_desc.pval_loc value_desc.pval_type
            value_desc.pval_attributes prim_name)
    in
    {
      resolved_type = pval_type;
      resolved_attributes = pval_attributes;
      resolved_name = prim_name;
      resolved_kind = Kind_external spec;
    }
  else
    {
      resolved_type = value_desc.pval_type;
      resolved_attributes = value_desc.pval_attributes;
      resolved_name = prim_name;
      resolved_kind = Kind_intrinsic;
    }

(* The built-in PPX, which every compilation runs before type checking, links
   this module, so the type checker always finds the resolver registered. *)
let () = Primitive.resolve_external := resolve

(* Resolve an FFI external now to report its errors and warnings, and keep it
   as written for the type checker, which resolves it again through
   [resolve]. Returns the external as written, its resolved form as a [val],
   and whether a relative [@module] makes it unfit for cross-module inlining.
   Such an external is exported as that [val], which the AST checks then see
   like any other; otherwise its resolved form is checked here, since the
   checks skip the unresolved one. *)
let validate (self : Ast_mapper.mapper) (prim : Parsetree.value_description) =
  let loc = prim.pval_loc in
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  match prim.pval_prim with
  | None -> Location.raise_errorf ~loc "empty primitive string"
  | Some {txt = prim_name} ->
    let resolution =
      Ast_external_process.resolve loc pval_type pval_attributes prim_name
    in
    let resolved_val : Parsetree.value_description =
      {
        prim with
        pval_type = resolution.pval_type;
        pval_prim = None;
        pval_attributes = resolution.pval_attributes;
      }
    in
    if not resolution.no_inline_cross_module then
      Bs_ast_invariant.check_resolved_external resolved_val;
    ( {prim with pval_type; pval_attributes},
      resolved_val,
      resolution.no_inline_cross_module )

let handle_external_in_sig (self : Ast_mapper.mapper)
    (prim : Parsetree.value_description) (sigi : Parsetree.signature_item) :
    Parsetree.signature_item =
  let external_, resolved_val, no_inline_cross_module = validate self prim in
  {
    sigi with
    psig_desc =
      Psig_value (if no_inline_cross_module then resolved_val else external_);
  }

let handle_external_in_stru (self : Ast_mapper.mapper)
    (prim : Parsetree.value_description) (str : Parsetree.structure_item) :
    Parsetree.structure_item =
  let external_, resolved_val, no_inline_cross_module = validate self prim in
  let external_result = {str with pstr_desc = Pstr_primitive external_} in
  if not no_inline_cross_module then external_result
  else
    let loc = prim.pval_loc in
    let open Ast_helper in
    Str.include_ ~loc
      (Incl.mk ~loc
         (Mod.constraint_ ~loc
            (Mod.structure ~loc [external_result])
            (Mty.signature ~loc
               [{psig_desc = Psig_value resolved_val; psig_loc = loc}])))
