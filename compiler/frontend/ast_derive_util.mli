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

val core_type_of_type_declaration :
  Parsetree.type_declaration -> Parsetree.core_type
(** Given a type declaration, extaract the type expression, mostly 
    used in code gen later
*)

val new_type_of_type_declaration :
  Parsetree.type_declaration ->
  string ->
  Parsetree.core_type * Parsetree.type_declaration

(* val mk_fun :
   loc:Location.t ->
   Parsetree.core_type ->
   string -> Parsetree.expression -> Parsetree.expression
   val destruct_label_declarations :
   loc:Location.t ->
   string ->
   Parsetree.label_declaration list ->
   (Parsetree.core_type * Parsetree.expression) list * string list *)

val not_applicable : Location.t -> string -> unit

val invalid_config : Parsetree.expression -> 'a
