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

val kind_of_lambda_block : Lam.t list -> Lam_id_kind.t

val field_flatten_get :
  (unit -> Lam.t) ->
  Ident.t ->
  int ->
  Lambda.field_dbg_info ->
  Lam_stats.ident_tbl ->
  Lam.t
(** [field_flattern_get cb v i tbl]
    try to remove the indirection of [v.(i)] by inlining when [v]
    is a known block, 
    if not, it will call [cb ()].

    Note due to different control flow, a constant block
    may result in out-of bound access, in that case, we should
    just ignore it. This does not mean our
    optimization is wrong, it means we hit an unreachable branch.
    for example
    {{
      let myShape = A 10 in 
      match myShape with 
      | A x -> x  (* only access field [0]*)
      | B (x,y) -> x + y (* Here it will try to access field [1] *)
    }}
*)

val alias_ident_or_global :
  Lam_stats.t -> Ident.t -> Ident.t -> Lam_id_kind.t -> unit

val refine_let : kind:Lam_compat.let_kind -> Ident.t -> Lam.t -> Lam.t -> Lam.t

val dump : string -> Lam.t -> unit
(** [dump] when {!Js_config.is_same_file}*)

val not_function : Lam.t -> bool

val is_function : Lam.t -> bool
