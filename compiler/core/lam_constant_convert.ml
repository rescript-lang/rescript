(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

let rec convert_constant (const : Lambda.structured_constant) : Lam_constant.t =
  match const with
  | Const_base (Const_int i) -> Const_int {i = Int32.of_int i; comment = None}
  | Const_base (Const_char i) -> Const_char i
  | Const_base (Const_string (s, opt)) ->
    let unicode =
      match opt with
      | Some opt -> Ast_utf8_string_interp.is_unicode_string opt
      | _ -> false
    in
    Const_string {s; unicode}
  | Const_base (Const_float i) -> Const_float i
  | Const_base (Const_int32 i) -> Const_int {i; comment = None}
  | Const_base (Const_int64 _) -> assert false
  | Const_base (Const_bigint (sign, i)) -> Const_bigint (sign, i)
  | Const_pointer (0, Pt_constructor {name = "()"; const = 1; non_const = 0}) ->
    Const_js_undefined {is_unit = true}
  | Const_false -> Const_js_false
  | Const_true -> Const_js_true
  | Const_pointer (i, p) -> (
    match p with
    | Pt_module_alias -> Const_module_alias
    | Pt_shape_none -> Lam_constant.lam_none
    | Pt_assertfalse -> Const_int {i = Int32.of_int i; comment = Pt_assertfalse}
    | Pt_constructor {name; const; non_const; attrs} ->
      let tag_type = Ast_untagged_variants.process_tag_type attrs in
      let i =
        match tag_type with
        | Some (Ast_untagged_variants.Int v) -> v
        | _ -> i
      in
      Const_int
        {
          i = Int32.of_int i;
          comment =
            Pt_constructor {cstr_name = {name; tag_type}; const; non_const};
        }
    | Pt_variant {name} ->
      if Ext_string.is_valid_hash_number name then
        Const_int {i = Ext_string.hash_number_as_i32_exn name; comment = None}
      else Const_pointer name)
  | Const_immstring s -> Const_string {s; unicode = false}
  | Const_block (t, xs) -> (
    let tag = Lambda.tag_of_tag_info t in
    match t with
    | Blk_some_not_nested ->
      Const_some (convert_constant (Ext_list.singleton_exn xs))
    | Blk_some -> Const_some (convert_constant (Ext_list.singleton_exn xs))
    | Blk_constructor _ | Blk_tuple | Blk_record _ | Blk_module _
    | Blk_module_export _ | Blk_extension | Blk_record_inlined _
    | Blk_record_ext _ ->
      Const_block (tag, t, Ext_list.map xs convert_constant)
    | Blk_poly_var s -> (
      match xs with
      | [_; value] ->
        let tag_val : Lam_constant.t =
          if Ext_string.is_valid_hash_number s then
            Const_int {i = Ext_string.hash_number_as_i32_exn s; comment = None}
          else Const_string {s; unicode = false}
        in
        Const_block (tag, t, [tag_val; convert_constant value])
      | _ -> assert false)
    | Blk_lazy_general -> assert false)
