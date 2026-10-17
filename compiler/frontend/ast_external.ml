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

let is_hoisted_component attrs =
  Option.is_some
    (Translattribute.get_empty_attribute "res.hoistedComponent" attrs)

let handle_external_in_sig (self : Ast_mapper.mapper)
    (prim : Parsetree.value_description) (sigi : Parsetree.signature_item) :
    Parsetree.signature_item =
  let loc = prim.pval_loc in
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  match prim.pval_prim with
  | None -> Location.raise_errorf ~loc "empty primitive string"
  | Some (Prim_ffi _ | Prim_inline_const _) ->
    Location.raise_errorf ~loc "external declaration already processed"
  | Some (Prim_name v) -> (
    match
      Ast_external_process.handle_attributes_as_prim loc pval_type
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
              pval_prim =
                (if no_inline_cross_module then None else Some pval_prim);
              pval_attributes;
            };
      })

let handle_external_in_stru (self : Ast_mapper.mapper)
    (prim : Parsetree.value_description) (str : Parsetree.structure_item) :
    Parsetree.structure_item =
  let loc = prim.pval_loc in
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  match prim.pval_prim with
  | None -> Location.raise_errorf ~loc "empty primitive string"
  | Some (Prim_ffi _ | Prim_inline_const _) ->
    Location.raise_errorf ~loc "external declaration already processed"
  | Some (Prim_name v) -> (
    match
      Ast_external_process.handle_attributes_as_prim loc pval_type
        pval_attributes v
    with
    | {pval_type; pval_prim; pval_attributes; no_inline_cross_module} ->
      let hoisted_component =
        no_inline_cross_module && is_hoisted_component pval_attributes
      in
      let external_result =
        {
          str with
          pstr_desc =
            Pstr_primitive
              {
                prim with
                pval_type;
                pval_prim = Some pval_prim;
                pval_attributes =
                  (if hoisted_component then [] else pval_attributes);
              };
        }
      in
      if hoisted_component then
        (* Relative FFI already exposes an ordinary value across module
           boundaries. Give that value a binding the hoist registry can track.
           Other FFI must keep its primitive type for existing interfaces. *)
        let open Ast_helper in
        let module_name = Location.mkloc "JsxExternal" loc in
        Str.value ~loc Nonrecursive
          [
            Vb.mk ~loc ~attrs:pval_attributes
              (Pat.var ~loc prim.pval_name)
              (Exp.letmodule ~loc
                 ~attrs:[(Location.mkloc "res.hoistedComponent" loc, PStr [])]
                 module_name
                 (Mod.structure ~loc [external_result])
                 (Exp.ident ~loc
                    {
                      txt = Ldot (Lident module_name.txt, prim.pval_name.txt);
                      loc;
                    }));
          ]
      else if not no_inline_cross_module then external_result
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
                             pval_prim = None;
                             pval_attributes;
                           };
                       psig_loc = loc;
                     };
                   ]))))
