(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

type ap_info = Lambda.ap_info = {
  ap_loc: Location.t;
  ap_inlined: Lambda.inline_attribute;
}

type ident = Ident.t

type lambda_switch = Lambda.lambda_switch

type apply = Lambda.lambda_apply

type lfunction = Lambda.lfunction

type prim_info = Lambda.prim_info

type t = Lambda.lambda
(** The optimizer's name for {!Lambda.lambda}. It is the same type: the
    constructors, their normalizations and the traversals live in Lambda, and
    the type is private there, so a term can only be built through them. *)

(**************************************************************)

val var : ident -> t

val global_module : ident -> t

val const : Lam_constant.t -> t

val apply : ?ap_transformed_jsx:bool -> t -> t list -> ap_info -> t

val function_ :
  loc:Location.t ->
  attr:Lambda.function_attribute ->
  params:ident list ->
  body:t ->
  t

val let_ : Lam_compat.let_kind -> ident -> t -> t -> t

val letrec : (ident * t) list -> t -> t

val if_ : t -> t -> t -> t

val switch : t -> lambda_switch -> t

val stringswitch : t -> (string * t) list -> t option -> t

val false_ : t

val unit : t

val sequor : t -> t -> t

val sequand : t -> t -> t

val not_ : Location.t -> t -> t

val seq : t -> t -> t

val break : t

val continue : t

val while_ : t -> t -> t

val try_ : t -> ident -> t -> t

val assign : ident -> t -> t

val prim : primitive:Lam_primitive.t -> args:t list -> Location.t -> t

val staticcatch : t -> int * ident list -> t -> t

val staticraise : int -> t list -> t

val for_ : ident -> t -> t -> Asttypes.direction_flag -> t -> t

val for_of : ident -> t -> t -> t

val for_await_of : ident -> t -> t -> t

(**************************************************************)

val shallow_map_sharing : (t -> t) -> t -> t

val eq_approx : t -> t -> bool
