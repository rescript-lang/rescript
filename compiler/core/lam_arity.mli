(* Copyright (C) Hongbo Zhang, Authors of ReScript
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

type t = private
  | Arity_info of int list * bool
      (**
     when the first argument is true, it is for sure 
     the last one means it can take any params later, 
     for an exception: it is (Arity_info([], true))
     approximation sound but not complete       
  *)
  | Arity_na

val equal : t -> t -> bool

val print : Format.formatter -> t -> unit

val print_arities_tbl : Format.formatter -> (Ident.t, t ref) Hashtbl.t -> unit

val merge : int -> t -> t

val non_function_arity_info : t

val raise_arity_info : t

val na : t

val info : int list -> bool -> t

val first_arity_na : t -> bool

val get_first_arity : t -> int option

val extract_arity : t -> int list
(** when [NA] return  empty list*)

val merge_arities : int list -> int list -> bool -> bool -> t
