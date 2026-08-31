(* Copyright (C) 2018- Hongbo Zhang, Authors of ReScript
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

type t =
  | Const_js_null
  | Const_js_undefined of {is_unit: bool}
  | Const_js_true
  | Const_js_false
  | Const_int of int32
  | Const_assertfalse
  | Const_constructor of Variant_runtime.tag
    (* Constant constructor of a nominal variant, emitted from its
         canonical runtime descriptor rather than an ordinal *)
  | Const_char of int
  | Const_string of {s: string; delim: External_arg_spec.delim option}
  | Const_float of string
  | Const_bigint of bool * string
  | Const_pointer of string
  | Const_block of Lambda.tag_info * t list
  | Const_some of t
  | Const_module_alias
(* eventually we can remove it, since we know
   [constant] is [undefined] or not
*)

let rec eq_approx (x : t) (y : t) =
  match x with
  | Const_module_alias -> y = Const_module_alias
  | Const_js_null -> y = Const_js_null
  | Const_js_undefined b -> y = Const_js_undefined b
  | Const_js_true -> y = Const_js_true
  | Const_js_false -> y = Const_js_false
  | Const_int ix -> (
    match y with
    | Const_int iy -> ix = iy
    | _ -> false)
  | Const_assertfalse -> y = Const_assertfalse
  | Const_constructor ix -> (
    match y with
    | Const_constructor iy -> ix = iy
    | _ -> false)
  | Const_char ix -> (
    match y with
    | Const_char iy -> ix = iy
    | _ -> false)
  | Const_string {s = sx; delim = ux} -> (
    match y with
    | Const_string {s = sy; delim = uy} -> sx = sy && ux = uy
    | _ -> false)
  | Const_float ix -> (
    match y with
    | Const_float iy -> ix = iy
    | _ -> false)
  | Const_bigint (sx, ix) -> (
    match y with
    | Const_bigint (sy, iy) -> sx = sy && ix = iy
    | _ -> false)
  | Const_pointer ix -> (
    match y with
    | Const_pointer iy -> ix = iy
    | _ -> false)
  | Const_block (ix, ixs) -> (
    match y with
    | Const_block (iy, iys) ->
      ix = iy && Ext_list.for_all2_no_exn ixs iys eq_approx
    | _ -> false)
  | Const_some ix -> (
    match y with
    | Const_some iy -> eq_approx ix iy
    | _ -> false)

let lam_none : t = Const_js_undefined {is_unit = false}

let rec is_allocating (c : t) : bool =
  match c with
  | Const_some t -> is_allocating t
  | Const_block _ -> true
  | Const_js_null | Const_js_undefined _ | Const_js_true | Const_js_false
  | Const_int _ | Const_assertfalse | Const_constructor _ | Const_char _
  | Const_string _ | Const_float _ | Const_bigint _ | Const_pointer _
  | Const_module_alias ->
    false
