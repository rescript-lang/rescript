(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

(** Define intemediate format to be serialized for cross module optimization
*)

(** In this module, 
    currently only arity information is  exported, 

    Short term: constant literals are also exported 

    Long term:
    Benefit? since Google Closure Compiler already did such huge amount of work
    TODO: simple expression, literal small function  can be stored, 
    but what would happen if small function captures other environment
    for example 

    {[
      let f  = fun x -> g x 
    ]}

    {[
      let f = g 
    ]}
*)

type arity = Single of Lam_arity.t | Submodule of Lam_arity.t array

type cmj_value = {
  arity: arity;
  persistent_closed_lambda: Lam.t option; (* Either constant or closed functor *)
}

type effect_ = string option

type keyed_cmj_value = {
  name: string;
  arity: arity;
  persistent_closed_lambda: Lam.t option;
}

type t = {
  values: keyed_cmj_value array;
  pure: bool;
  package_spec: Js_packages_info.t;
  case: Ext_js_file_kind.case;
}

val make :
  values:cmj_value Map_string.t ->
  effect_:effect_ ->
  package_spec:Js_packages_info.t ->
  case:Ext_js_file_kind.case ->
  t

val query_by_name : t -> string -> keyed_cmj_value

val single_na : arity

val from_file : string -> t

val from_file_with_digest : string -> t * Digest.t

val from_string : string -> t

(*
   Note writing the file if its content is not changed
*)
val to_file : string -> check_exists:bool -> t -> unit

type path = string

type cmj_load_info = {cmj_table: t; package_path: path}
