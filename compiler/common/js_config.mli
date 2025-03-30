(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type jsx_version = Jsx_v4
type jsx_module = React | Generic of {module_name: string}

(* val get_packages_info :
   unit -> Js_packages_info.t *)

val no_version_header : bool ref
(** set/get header *)

val directives : string list ref
(** directives printed verbatims just after the version header *)

(** return [package_name] and [path] 
    when in script mode: 
*)

(* val get_current_package_name_and_path :
   Js_packages_info.module_system ->
   Js_packages_info.info_query *)

(* val set_package_name : string -> unit
   val get_package_name : unit -> string option *)

val cross_module_inline : bool ref
(** cross module inline option *)

val diagnose : bool ref
(** diagnose option *)

val no_builtin_ppx : bool ref
(** options for builtin ppx *)

val check_div_by_zero : bool ref
(** check-div-by-zero option *)

val get_check_div_by_zero : unit -> bool

val tool_name : string

val syntax_only : bool ref

val binary_ast : bool ref

val debug : bool ref

val cmi_only : bool ref

val cmj_only : bool ref

(* stopped after generating cmj *)
val force_cmi : bool ref

val force_cmj : bool ref

val jsx_version : jsx_version option ref

val jsx_module : jsx_module ref

val js_stdout : bool ref

val all_module_aliases : bool ref

val no_stdlib : bool ref

val no_export : bool ref

val int_of_jsx_version : jsx_version -> int

val string_of_jsx_module : jsx_module -> string

val jsx_version_of_int : int -> jsx_version option

val jsx_module_of_string : string -> jsx_module

val customize_runtime : string option ref

val as_pp : bool ref

val self_stack : string Stack.t
