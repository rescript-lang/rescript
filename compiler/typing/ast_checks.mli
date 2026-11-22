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

(** Checks on the parsetree that the type checker doesn't make: unused
    compiler attributes, removed OCaml spellings, integer literals out of
    range, redefined built-in types and constructors, and the arity of
    [%identity] externals. The frontend runs them over each file after its
    PPXes; the type checker runs them over each FFI external in its resolved
    form. *)

val warn_discarded_unused_attributes : Parsetree.attributes -> unit
(** [warn_discarded_unused_attributes discarded] warns about each compiler
    attribute in [discarded] that nothing used, and rejects a removed
    [ocaml.]-prefixed spelling. *)

val check_removed_ocaml_attribute : Parsetree.attribute -> unit

val check_removed_ocaml_extension : Parsetree.extension -> unit

val iterator : Ast_iterator.iterator
(** The walk that reports all the checks. It skips FFI externals: the
    declaration stays as written until type checking, so its argument types
    still carry the attributes that resolution consumes. [Ast_external]
    checks each one in its resolved form with [check_resolved_external]. *)

val check_resolved_external :
  Parsetree.value_description ->
  pval_type:Parsetree.core_type ->
  pval_attributes:Parsetree.attributes ->
  unit
(** Check an external in its resolved form: the declaration with the type and
    the attributes resolution left *)
