(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

(* let derivingName = "abstract" *)
module U = Ast_derive_util
open Ast_helper
(* type tdcls = Parsetree.type_declaration list *)

type abstract_kind = Not_abstract | Light_abstract | Complex_abstract

let is_abstract (xs : Ast_payload.action list) =
  match xs with
  | [({txt = "abstract"}, None)] -> Complex_abstract
  | [({txt = "abstract"}, Some {pexp_desc = Pexp_ident {txt = Lident "light"}})]
    ->
    Light_abstract
  | [({loc; txt = "abstract"}, Some _)] ->
    Location.raise_errorf ~loc "invalid config for abstract"
  | xs ->
    Ext_list.iter xs (function {loc; txt}, _ ->
        (match txt with
        | "abstract" ->
          Location.raise_errorf ~loc
            "deriving abstract does not work with any other deriving"
        | _ -> ()));
    Not_abstract
(* let handle_config (config : Parsetree.expression option) =
   match config with
   | Some config ->
    U.invalid_config config
   | None -> () *)

(** For this attributes, its type was wrapped as an option,
    so we can still reuse existing frame work
*)
let get_optional_attrs =
  [Ast_attributes.get; Ast_attributes.bs_return_undefined]

let get_attrs = []

let set_attrs = [Ast_attributes.set]

let handle_tdcl light (tdcl : Parsetree.type_declaration) :
    Parsetree.type_declaration * Parsetree.value_description list =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let new_tdcl =
    {
      tdcl with
      ptype_kind = Ptype_abstract;
      ptype_attributes = [] (* avoid non-terminating*);
    }
  in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      Ext_list.exists label_declarations (fun x ->
          Ast_attributes.has_bs_optional x.pld_attributes)
    in
    let setter_accessor, make_type, labels =
      Ext_list.fold_right label_declarations
        ( [],
          (if has_optional_field then
             Ast_compatible.arrow ~loc ~arity:None (Ast_literal.type_unit ())
               core_type
           else core_type),
          [] )
        (fun ({
                pld_name = {txt = label_name; loc = label_loc} as pld_name;
                pld_type;
                pld_mutable;
                pld_attributes;
                pld_loc;
              } :
               Parsetree.label_declaration)
             (acc, maker, labels)
           ->
          let prim_as_name, new_label =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None -> (label_name, pld_name)
            | Some new_name -> (new_name, {pld_name with txt = new_name})
          in
          let prim = [prim_as_name] in
          let is_optional = Ast_attributes.has_bs_optional pld_attributes in

          let maker, acc =
            let arity =
              if List.length labels = List.length label_declarations - 1 then
                (* toplevel type *)
                Some ((if has_optional_field then 2 else 1) + List.length labels)
              else None
            in
            if is_optional then
              let optional_type = Ast_core_type.lift_option_type pld_type in
              ( Ast_compatible.opt_arrow ~loc:pld_loc ~arity label_name pld_type
                  maker,
                Val.mk ~loc:pld_loc
                  (if light then pld_name
                   else {pld_name with txt = pld_name.txt ^ "Get"})
                  ~attrs:get_optional_attrs ~prim
                  (Ast_compatible.arrow ~loc ~arity:(Some 1) core_type
                     optional_type)
                :: acc )
            else
              ( Ast_compatible.label_arrow ~loc:pld_loc ~arity label_name
                  pld_type maker,
                Val.mk ~loc:pld_loc
                  (if light then pld_name
                   else {pld_name with txt = pld_name.txt ^ "Get"})
                  ~attrs:get_attrs
                  ~prim:
                    ((* Not needed actually*)
                     External_ffi_types.ffi_bs_as_prims
                       [External_arg_spec.dummy] Return_identity
                       (Js_get {js_get_name = prim_as_name; js_get_scopes = []}))
                  (Ast_compatible.arrow ~loc ~arity:(Some 1) core_type pld_type)
                :: acc )
          in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                Ast_compatible.arrow ~arity:(Some 2) core_type
                  (Ast_compatible.arrow ~arity:None pld_type (* setter *)
                     (Ast_literal.type_unit ()))
              in
              Val.mk ~loc:pld_loc
                {loc = label_loc; txt = label_name ^ "Set"} (* setter *)
                ~attrs:set_attrs ~prim setter_type
              :: acc
            else acc
          in
          (acc, maker, (is_optional, new_label) :: labels))
    in
    ( new_tdcl,
      if is_private then setter_accessor
      else
        let my_prims =
          Ast_external_process.pval_prim_of_option_labels labels
            has_optional_field
        in
        let my_maker =
          Val.mk ~loc {loc; txt = type_name} ~prim:my_prims make_type
        in
        my_maker :: setter_accessor )
  | Ptype_abstract | Ptype_variant _ | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    (tdcl, [])

let handle_tdcls_in_str ~light rf tdcls =
  let tdcls, code =
    Ext_list.fold_right tdcls ([], []) (fun tdcl (tdcls, sts) ->
        match handle_tdcl light tdcl with
        | ntdcl, value_descriptions ->
          ( ntdcl :: tdcls,
            Ext_list.map_append value_descriptions sts (fun x ->
                Str.primitive x) ))
  in
  Ast_compatible.rec_type_str rf tdcls :: code
(* still need perform transformation for non-abstract type*)

let handle_tdcls_in_sig ~light rf tdcls =
  let tdcls, code =
    Ext_list.fold_right tdcls ([], []) (fun tdcl (tdcls, sts) ->
        match handle_tdcl light tdcl with
        | ntdcl, value_descriptions ->
          ( ntdcl :: tdcls,
            Ext_list.map_append value_descriptions sts (fun x -> Sig.value x) ))
  in
  Ast_compatible.rec_type_sig rf tdcls :: code
