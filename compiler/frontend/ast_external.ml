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

let handle_external_in_sig (self : Bs_ast_mapper.mapper)
    (prim : Parsetree.value_description) (sigi : Parsetree.signature_item) :
    Parsetree.signature_item =
  let loc = prim.pval_loc in
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  match prim.pval_prim with
  | [] -> Location.raise_errorf ~loc "empty primitive string"
  | a :: b :: _ ->
    Location.raise_errorf ~loc
      "only a single string is allowed in bs external %S %S" a b
  | [v] -> (
    match
      Ast_external_process.encode_attributes_as_string loc pval_type
        pval_attributes v
    with
    | {pval_type; pval_prim; pval_attributes; no_inline_cross_module} ->
      {
        sigi with
        psig_desc =
          Psig_value
            {
              prim with
              pval_type;
              pval_prim = (if no_inline_cross_module then [] else pval_prim);
              pval_attributes;
            };
      })

let handle_external_in_stru (self : Bs_ast_mapper.mapper)
    (prim : Parsetree.value_description) (str : Parsetree.structure_item) :
    Parsetree.structure_item =
  let loc = prim.pval_loc in
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  match prim.pval_prim with
  | [] -> Location.raise_errorf ~loc "empty primitive string"
  | a :: b :: _ ->
    Location.raise_errorf ~loc
      "only a single string is allowed in bs external %S : %S" a b
  | [v] -> (
    match
      Ast_external_process.encode_attributes_as_string loc pval_type
        pval_attributes v
    with
    | {pval_type; pval_prim; pval_attributes; no_inline_cross_module} ->
      let external_result =
        {
          str with
          pstr_desc =
            Pstr_primitive {prim with pval_type; pval_prim; pval_attributes};
        }
      in
      if not no_inline_cross_module then external_result
      else
        let open Ast_helper in
        Str.include_ ~loc
          (Incl.mk ~loc
             (Mod.constraint_ ~loc
                (Mod.structure ~loc [external_result])
                (Mty.signature ~loc
                   [
                     {
                       psig_desc =
                         Psig_value
                           {
                             prim with
                             pval_type;
                             pval_prim = [];
                             pval_attributes;
                           };
                       psig_loc = loc;
                     };
                   ]))))
