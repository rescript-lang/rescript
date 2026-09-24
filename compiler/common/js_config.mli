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
type source_map = No_source_map | Linked | Inline | Hidden

(* val get_packages_info :
   unit -> Js_packages_info.t *)

type t = {
  no_version_header: bool ref;  (** Suppress the version header. *)
  directives: string list ref;
      (** Directives printed verbatim just after the version header. *)
  cross_module_inline: bool ref;  (** Cross-module inlining option. *)
  debug_ir: bool ref;
      (** Dump intermediate representations and related diagnostics. *)
  check_lam: bool ref;
      (** Check Lambda invariants after optimization passes. *)
  no_builtin_ppx: bool ref;
  check_div_by_zero: bool ref;
  syntax_only: bool ref;
  binary_ast: bool ref;
  test_ast_conversion: bool ref;
  debug: bool ref;
  cmi_only: bool ref;
  cmj_only: bool ref;
  force_cmi: bool ref;
  force_cmj: bool ref;
  jsx_version: jsx_version option ref;
  jsx_module: jsx_module ref;
  jsx_preserve: bool ref;
  js_stdout: bool ref;
  source_map: source_map ref;
  source_map_sources_content: bool ref;
  source_map_root: string ref;
  all_module_aliases: bool ref;
  no_stdlib: bool ref;
  no_export: bool ref;
  as_pp: bool ref;
  self_stack: string Stack.t;
}

val current : unit -> t
(* Mutable settings for the current domain's compiler request. *)

val with_fresh : (unit -> 'a) -> 'a
(* Run with default settings and restore the previous request on exit. *)

val tool_name : string

val int_of_jsx_version : jsx_version -> int

val string_of_jsx_module : jsx_module -> string

val jsx_version_of_int : int -> jsx_version option

val jsx_module_of_string : string -> jsx_module

val reset : unit -> unit
