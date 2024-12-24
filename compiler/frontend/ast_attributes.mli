(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
type attr = Parsetree.attribute

type t = attr list

type ('a, 'b) st = {get: 'a option; set: 'b option}

val process_method_attributes_rev : t -> (bool * bool, [`Get | `No_get]) st * t

type attr_kind = Nothing | Meth_callback of attr | Method of attr

val process_attributes_rev : t -> attr_kind * t

val has_inline_payload : t -> attr option

val has_await_payload : t -> attr option

type derive_attr = {bs_deriving: Ast_payload.action list option} [@@unboxed]

val iter_process_bs_string_int_unwrap_uncurry :
  t -> [`Nothing | `String | `Int | `Ignore | `Unwrap]

val iter_process_bs_string_as : t -> string option

val has_bs_optional : t -> bool

val iter_process_bs_int_as : t -> int option

type as_const_payload = Int of int | Str of string * External_arg_spec.delim
val iter_process_bs_string_or_int_as : t -> as_const_payload option

val process_derive_type : t -> derive_attr * t

val get : attr

val get_index : attr

val set : attr

val bs_return_undefined : attr

val internal_expansive : attr
(* val deprecated : string -> attr *)

val rs_externals : t -> string list -> bool

val is_gentype : attr -> bool

val gentype : attr
