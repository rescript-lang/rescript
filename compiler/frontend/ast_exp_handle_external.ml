(* Copyright (C) 2020 Hongbo Zhang, Authors of ReScript
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

let handle_debugger loc (payload : Ast_payload.t) =
  match payload with
  | PStr [] ->
    Ast_external_mk.local_external_apply loc ~pval_prim:"%debugger"
      ~pval_type:
        (Ast_helper.Typ.arrow
           [Parg_type {attrs = []; lbl = Nolabel; typ = Ast_helper.Typ.any ()}]
           (Ast_literal.type_unit ()))
      [Ast_literal.val_unit ~loc ()]
  | _ ->
    Location.raise_errorf ~loc "%%debugger extension doesn't accept arguments"

let raw_type () =
  Ast_helper.Typ.arrow
    [Parg_type {attrs = []; lbl = Nolabel; typ = Ast_helper.Typ.any ()}]
    (Ast_helper.Typ.any ())

(* [exp] is the string constant holding the JavaScript source. *)
let raw_expression loc ~is_function (exp : Parsetree.expression) =
  {
    exp with
    pexp_desc =
      Ast_external_mk.local_external_apply loc ~pval_prim:"#raw_expr"
        ~pval_type:(raw_type ()) [exp];
    pexp_attributes =
      (match !is_function with
      | None -> exp.pexp_attributes
      | Some _ -> Ast_attributes.internal_expansive :: exp.pexp_attributes);
  }

let ffi_error loc =
  Location.raise_errorf ~loc
    "%%ffi extension can only be applied to a string containing a JavaScript \
     function such as \"(x) => ...\""

let ffi_expression loc ~is_function (exp : Parsetree.expression) =
  (* Wrap a type constraint based on arity.
     E.g. for arity 2 constrain to type (_, _) => _ *)
  let wrap_type_constraint (e : Parsetree.expression) =
    let loc = e.pexp_loc in
    let any = Ast_helper.Typ.any ~loc:e.pexp_loc () in
    let arrow ~arity =
      let effective_arity = if arity = 0 then 1 else arity in
      let args =
        Ext_list.init effective_arity (fun _ ->
            Parsetree.Parg_type {attrs = []; lbl = Nolabel; typ = any})
      in
      Ast_helper.Typ.arrow ~loc args any
    in
    match !is_function with
    | Some arity -> Ast_helper.Exp.constraint_ ~loc e (arrow ~arity)
    | _ -> ffi_error loc
  in
  match !is_function with
  | None -> ffi_error loc
  | Some _ -> wrap_type_constraint (raw_expression loc ~is_function exp)

let raw_structure loc (exp : Parsetree.expression) =
  Ast_helper.Str.eval
    {
      exp with
      pexp_desc =
        Ast_external_mk.local_external_apply loc ~pval_prim:"#raw_stmt"
          ~pval_type:(raw_type ()) [exp];
    }

let handle_raw loc payload =
  let is_function = ref None in
  match
    Ast_payload.raw_as_string_exp_exn ~kind:Raw_exp ~is_function payload
  with
  | None ->
    Location.raise_errorf ~loc "%%raw extension can only be applied to a string"
  | Some exp -> raw_expression loc ~is_function exp

let handle_ffi ~loc ~payload =
  let is_function = ref None in
  match
    Ast_payload.raw_as_string_exp_exn ~kind:Raw_exp ~is_function payload
  with
  | None -> ffi_error loc
  | Some exp -> ffi_expression loc ~is_function exp

let handle_raw_structure loc payload =
  match Ast_payload.raw_as_string_exp_exn ~kind:Raw_program payload with
  | Some exp -> raw_structure loc exp
  | None ->
    Location.raise_errorf ~loc
      "%%%%raw extension can only be applied to a string"

(* The string constant holding the JavaScript source of a raw node, validated
   like a raw payload. *)
let raw_source_expression ~(kind : Js_raw_info.raw_kind) ?is_function
    (source : Parsetree.raw_source) =
  let str = Ast_raw.js_source source in
  let loc = Ast_raw.payload_loc source in
  Ast_payload.validate_raw_source ~kind ?is_function ~loc
    ~offset:(Bs_flow_ast_utils.flow_deli_offset (Some "js"))
    str;
  Ast_helper.Exp.constant ~loc (Ast_helper.Const.string str)

let handle_raw_node ~loc (kind : Parsetree.raw_kind) source =
  let is_function = ref None in
  let exp = raw_source_expression ~kind:Raw_exp ~is_function source in
  match kind with
  | Raw_expression -> raw_expression loc ~is_function exp
  | Raw_function -> ffi_expression loc ~is_function exp

let handle_raw_structure_node ~loc source =
  raw_structure loc (raw_source_expression ~kind:Raw_program source)
