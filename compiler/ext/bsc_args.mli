(* Copyright (C) 2020- Authors of ReScript
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

type anon_fun = rev_args:string list -> unit

type string_action =
  | String_call of (string -> unit)
  | String_set of string ref
  | String_optional_set of string option ref
  | String_list_add of string list ref

type unit_action =
  | Unit_call of (unit -> unit)
  | Unit_lazy of unit lazy_t
  | Unit_set of bool ref
  | Unit_clear of bool ref

type spec = Unit_dummy | Unit of unit_action | String of string_action

type t = (string * spec * string) array

exception Bad of string

val bad_arg : string -> 'a

val parse_exn :
  usage:string ->
  argv:string array ->
  ?start:int ->
  ?finish:int ->
  t ->
  (rev_args:string list -> unit) ->
  unit
