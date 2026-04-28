(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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

module E = Js_exp_make

(** return [val < 0] if not nested [Some (Some (Some None))]*)
let rec is_some_none_aux (x : Lam_constant.t) acc =
  match x with
  | Const_some v -> is_some_none_aux v (acc + 1)
  | Const_module_alias | Const_js_undefined _ -> acc
  | _ -> -1

let rec nested_some_none n none =
  if n = 0 then none else nested_some_none (n - 1) (E.optional_block none)

let rec translate_some (x : Lam_constant.t) : J.expression =
  let depth = is_some_none_aux x 0 in
  if depth < 0 then E.optional_not_nest_block (translate x)
  else
    nested_some_none depth
      (E.optional_block (translate (Const_js_undefined {is_unit = false})))

and translate (x : Lam_constant.t) : J.expression =
  match x with
  | Const_module_alias -> E.undefined (*  TODO *)
  | Const_some s -> translate_some s
  | Const_js_true -> E.bool true
  | Const_js_false -> E.bool false
  | Const_js_null -> E.nil
  | Const_js_undefined {is_unit = true} -> E.unit
  | Const_js_undefined {is_unit = false} -> E.undefined
  | Const_int
      {i; comment = Pt_constructor {cstr_name = {name; tag_type = None}}}
    when name <> "[]" ->
    E.str name
  | Const_int {i; comment = Pt_constructor {cstr_name = {tag_type = Some t}}} ->
    E.tag_type t
  | Const_int {i; comment} ->
    E.int i ?comment:(Lam_constant.string_of_pointer_info comment)
  | Const_char i -> Js_of_lam_string.const_char i
  | Const_bigint (sign, i) -> E.bigint sign i
  | Const_float f -> E.float f (* TODO: preserve float *)
  | Const_string {s; delim = None | Some DNoQuotes} -> E.str s
  | Const_string {s; delim = Some delim} -> E.str ~delim s
  | Const_pointer name -> E.str name
  | Const_block (tag, tag_info, xs) ->
    Js_of_lam_block.make_block NA tag_info (E.small_int tag)
      (Ext_list.map xs translate)

(* E.arr Mutable ~comment:"float array" *)
(*   (Ext_list.map (fun x ->  E.float  x ) ars) *)

(* and translate_optional s =
   let  b =
   match s with
   | Const_js_undefined -> E.optional_block (translate s) *)

let translate_arg_cst (cst : External_arg_spec.cst) =
  match cst with
  | Arg_int_lit i -> E.int (Int32.of_int i)
  | Arg_string_lit (s, delim) -> E.str s ~delim
