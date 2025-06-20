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

(* note we first declare its type is [unit],
   then [ignore] it, [ignore] is necessary since
   the js value  maybe not be of type [unit] and
   we can use [unit] value (though very little chance)
   sometimes
*)
(* val discard_exp_as_unit :
   Location.t -> Parsetree.expression -> Parsetree.expression *)

val tuple_type_pair :
  ?loc:Ast_helper.loc ->
  [< `Make | `Run] ->
  int ->
  Parsetree.core_type * Parsetree.core_type list * Parsetree.core_type

val to_undefined_type : Location.t -> Parsetree.core_type -> Parsetree.core_type

val to_regexp_type : Location.t -> Parsetree.core_type

val single_non_rec_value :
  ?attrs:Parsetree.attributes ->
  Ast_helper.str ->
  Parsetree.expression ->
  Parsetree.structure_item

val single_non_rec_val :
  ?attrs:Parsetree.attributes ->
  Ast_helper.str ->
  Parsetree.core_type ->
  Parsetree.signature_item
