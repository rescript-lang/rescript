(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(** Creator utilities for the [J] module *)

type t = J.statement

(** empty statement, block of length 0 *)
(* val empty_stmt :
   t *)

val throw_stmt : ?comment:string -> J.expression -> t

val if_ :
  ?comment:string ->
  ?declaration:Lam_compat.let_kind * Ident.t ->
  (* when it's not None, we also need make a variable declaration in the
     begininnig, however, we can optmize such case
  *)
  ?else_:J.block ->
  J.expression ->
  J.block ->
  t

val block : ?comment:string -> J.block -> t
(**   
   turn a block into  a single statement,
   avoid nested block
*)

val int_switch :
  ?comment:string ->
  ?declaration:Lam_compat.let_kind * Ident.t ->
  ?default:J.block ->
  J.expression ->
  (int * J.case_clause) list ->
  t
(** [int_switch ~declaration e clauses]

    The [declaration] is attached to peepwhole 
    such pattern 

    {[
      var x ;
      x = yy
    ]}

    into
    {[
      var x = yy;
    ]}
*)

val string_switch :
  ?comment:string ->
  ?declaration:Lam_compat.let_kind * Ident.t ->
  ?default:J.block ->
  J.expression ->
  (Ast_untagged_variants.tag_type * J.case_clause) list ->
  t

val declare_variable :
  ?comment:string ->
  ?ident_info:J.ident_info ->
  kind:Lam_compat.let_kind ->
  Ident.t ->
  t
(** Just declaration without initialization *)

(*** Declaration with initialization *)
val define_variable :
  ?comment:string ->
  ?ident_info:J.ident_info ->
  kind:Lam_compat.let_kind ->
  Ident.t ->
  J.expression ->
  t

(** created an alias expression *)
(* val alias_variable :
   ?comment:string ->
   exp:J.expression ->
   Ident.t ->
   t *)

val assign : ?comment:string -> J.ident -> J.expression -> t

(** Used in cases like 
    {[
      let x = while true do 
          ...
        done in ..
    ]}
*)
(* val assign_unit :
   ?comment:string  ->
   J.ident ->
   t *)

(** used in cases like 
    {[
      let x = while true do 
          ...
        done in ..
    ]}
*)
(* val declare_unit :
   ?comment:string  ->
   J.ident ->
   t *)

val while_ : ?comment:string -> J.expression -> J.block -> t

val for_ :
  ?comment:string ->
  J.for_ident_expression option ->
  J.finish_ident_expression ->
  J.for_ident ->
  J.for_direction ->
  J.block ->
  t

val try_ :
  ?comment:string ->
  ?with_:J.ident * J.block ->
  ?finally:J.block ->
  J.block ->
  t

val exp : ?comment:string -> J.expression -> t

val return_stmt : ?comment:string -> J.expression -> t

(* val return_unit : t list *)
(** for ocaml function which returns unit 
    it will be compiled into [return 0] in js *)

(** if [label] is not set, it will default to empty *)
(* val continue_stmt :
   ?comment:string  ->
   ?label:J.label ->
   unit  ->
   t *)

val continue_ : t

val debugger_block : t list
