(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

type warning_error =
  | Warn_error_false
  (* default [false] to make our changes non-intrusive *)
  | Warn_error_true
  | Warn_error_number of string

type t0 = {number: string option; error: warning_error}

type nonrec t = t0 option

val to_merlin_string : t -> string
(** Extra work is need to make merlin happy *)

val from_map : Ext_json_types.t Map_string.t -> t

val to_bsb_string : package_kind:Bsb_package_kind.t -> t -> string
(** [to_bsb_string not_dev warning]
*)

val use_default : t
