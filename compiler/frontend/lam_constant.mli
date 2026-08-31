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
  (* eventually we can remove it, since we know
     [constant] is [undefined] or not
  *)
  | Const_module_alias

val eq_approx : t -> t -> bool

val lam_none : t

val is_allocating : t -> bool
