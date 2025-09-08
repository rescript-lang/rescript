(* Copyright (C) 2017 Authors of ReScript
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

val map_row_fields_into_ints :
  Location.t -> Parsetree.row_field list -> (string * int) list
(** side effect: it will mark used attributes `as`  *)

val map_constructor_declarations_into_ints :
  Parsetree.constructor_declaration list -> [`Offset of int | `New of int list]

val map_row_fields_into_strings :
  Location.t -> Parsetree.row_field list -> External_arg_spec.attr

(* val is_enum :
   Parsetree.row_field list ->
   bool *)

val is_enum_polyvar :
  Parsetree.type_declaration -> Parsetree.row_field list option
