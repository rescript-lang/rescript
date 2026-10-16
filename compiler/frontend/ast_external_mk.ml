(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 -  Hongbo Zhang, Authors of ReScript
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

let local_external_apply loc ?(pval_attributes = []) ~(pval_prim : string)
    ~(pval_type : Parsetree.core_type) ?(local_module_name = "J")
    ?(local_fun_name = "unsafe_expr") (args : Parsetree.expression list) :
    Parsetree.expression_desc =
  Pexp_letmodule
    ( {txt = local_module_name; loc},
      Ast_helper.Mod.structure ~loc
        [
          Ast_helper.Str.primitive ~loc
            {
              pval_name = {txt = local_fun_name; loc};
              pval_type;
              pval_loc = loc;
              pval_prim = Some {txt = pval_prim; loc};
              pval_attributes;
            };
        ],
      Ast_helper.Exp.apply ~loc
        (Ast_helper.Exp.ident ~loc
           {txt = Ldot (Lident local_module_name, local_fun_name); loc})
        (Ext_list.map args (fun x -> (Asttypes.Nolabel, x))) )

let inline_const_prim = "#rescript-inline"

let inline_const_of_expression (expression : Parsetree.expression) :
    External_ffi_types.inline_const option =
  match (Ast_payload.unwrap_braces expression).pexp_desc with
  | Pexp_constant (Pconst_string _)
  | Pexp_template {source_segments = [_]; values = []} -> (
    match Ast_payload.semantic_string_of_expression expression with
    | Some semantic -> Some (Const_string semantic)
    | None -> assert false)
  | Pexp_constant (Pconst_integer (s, None)) ->
    Some (Const_int (Int32.of_string s))
  | Pexp_constant (Pconst_integer (s, Some 'n')) ->
    let positive, digits = Bigint_utils.parse_bigint s in
    Some (Const_bigint {positive; digits})
  | Pexp_constant (Pconst_float (s, None)) -> Some (Const_float s)
  | Pexp_construct ({txt = Lident (("true" | "false") as txt)}, {txt = []}) ->
    Some (Const_bool (txt = "true"))
  | _ -> None

let inline_const_declaration ~(attr_loc : Location.t)
    (value_desc : Parsetree.value_description)
    (expression : Parsetree.expression) : Parsetree.value_description =
  {
    value_desc with
    pval_prim = Some {txt = inline_const_prim; loc = attr_loc};
    pval_attributes =
      [
        ( {txt = "inline"; loc = attr_loc},
          PStr [Ast_helper.Str.eval ~loc:expression.pexp_loc expression] );
      ];
  }

let inline_const_of_declaration (value_desc : Parsetree.value_description) =
  match value_desc.pval_attributes with
  | [({txt = "inline"}, PStr [{pstr_desc = Pstr_eval (expression, _)}])] ->
    inline_const_of_expression expression
  | _ -> None
