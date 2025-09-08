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

(** The intemediate output when compiling lambda into JS IR *)

(* Hongbo Should we rename this module js_of_lambda since it looks like it's
   containing that step
*)

type finished = True | False | Dummy
(* Have no idea, so that when [++] is applied, always use the other *)

type t = {block: J.block; value: J.expression option; output_finished: finished}

(** When [finished] is true the block is already terminated,
    value does not make sense
    [finished]  default to false, which is conservative
*)

val make : ?value:J.expression -> ?output_finished:finished -> J.block -> t

val output_as_block : t -> J.block

val to_break_block : t -> J.block * bool
(* the second argument is
   [true] means [break] needed

   When we know the output is gonna finished true
   we can reduce
   {[
     return xx ;
     break
   ]}
   into
   {[
     return ;
   ]}
*)

val append_output : t -> t -> t

val dummy : t

val output_of_expression :
  Lam_compile_context.continuation ->
  J.expression ->
  (* compiled expression *)
  no_effects:bool Lazy.t ->
  t

val output_of_block_and_expression :
  Lam_compile_context.continuation -> J.block -> J.expression -> t
(** - needed for instrument [return] statement properly
*)

val concat : t list -> t

val to_string : t -> string
