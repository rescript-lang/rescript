(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2016 - Hongbo Zhang, Authors of ReScript
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

(** Analyzing utilities for [J] module *)

(** for example, whether it has side effect or not.
*)

val free_variables_of_statement : J.statement -> Set_ident.t

val free_variables_of_expression : J.expression -> Set_ident.t

(* val no_side_effect_expression_desc :
   J.expression_desc -> bool *)

val no_side_effect_expression : J.expression -> bool
(** [no_side_effect] means this expression has no side effect, 
    but it might *depend on value store*, so you can not just move it around,

    for example,
    when you want to do a deep copy, the expression passed to you is pure
    but you still have to call the function to make a copy, 
    since it maybe changed later
*)

val no_side_effect_statement : J.statement -> bool
(** 
    here we say 
   {[ var x = no_side_effect_expression ]}
    is [no side effect], but it is actually side effect, 
    since  we are defining a variable, however, if it is not exported or used, 
    then it's fine, so we delay this check later
*)

val eq_expression : J.expression -> J.expression -> bool

val eq_statement : J.statement -> J.statement -> bool

val eq_block : J.block -> J.block -> bool

val rev_flatten_seq : J.expression -> J.block

val rev_toplevel_flatten : J.block -> J.block
(** return the block in reverse order *)

(* val is_constant : J.expression -> bool *)

(** Simple expression, 
    no computation involved so that  it is okay to be duplicated
*)

val is_okay_to_duplicate : J.expression -> bool
