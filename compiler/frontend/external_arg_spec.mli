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

type delim = DNone | DStarJ | DNoQuotes | DBackQuotes

type cst = private Arg_int_lit of int | Arg_string_lit of string * delim

type attr =
  | Poly_var_string of {descr: (string * string) list}
  | Poly_var of {descr: (string * string) list option}
  | Int of (string * int) list (* ([`a | `b ] [@int])*)
  | Arg_cst of cst
  (* maybe we can improve it as a combination of {!Asttypes.constant} and tuple *)
  | Extern_unit
  | Nothing
  | Ignore
  | Unwrap

type label_noname = Arg_label | Arg_empty | Arg_optional

type label = private
  | Obj_empty
  | Obj_label of {name: string}
  | Obj_optional of {name: string; for_sure_no_nested_option: bool}
(* it will be ignored , side effect will be recorded *)

type obj_param = {obj_arg_type: attr; obj_arg_label: label}

type param = {arg_type: attr; arg_label: label_noname}

type obj_params = obj_param list

type params = param list

val cst_int : int -> cst

val cst_string : string -> delim -> cst

val empty_label : label

(* val empty_lit : cst -> label  *)
val obj_label : string -> label

val optional : bool -> string -> label

val empty_kind : attr -> obj_param

val dummy : param
