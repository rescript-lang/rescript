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

(** Helper for global Ocaml module index into meaningful names  *)

val reset : unit -> unit

val add_js_module :
  ?import_attributes:External_ffi_types.import_attributes ->
  External_ffi_types.module_bind_name ->
  string ->
  bool ->
  dynamic_import:bool ->
  Ident.t
(** 
   [add_js_module hint_name module_name]
   Given a js module name and hint name, assign an id to it
   we also bookkeep it as [External] dependency.

   Note the complexity lies in that we should consolidate all 
   same external dependencies into a single dependency.

   The strategy is that we first create a [Lam_module_ident.t] 
   and  query it if already exists in [cache_tbl], if it already
   exists, we discard the freshly made one, and use the cached one,
   otherwise, use the freshly made one instead

   Invariant: 
    any [id] as long as put in the [cached_tbl] should be always valid,
*)

(* The other dependencies are captured by querying
   either when [access] or when expansion,
   however such dependency can be removed after inlining etc.

   When we register such compile time dependency we classified
   it as
   Visit (ml), Builtin(built in js), External()

   For external, we never remove, we only consider
   remove dependency for Runtime and Visit, so
   when compile OCaml to Javascript, we only need
   pay attention to for those modules are actually used or not
*)

val query_external_id_info :
  ?dynamic_import:bool -> Ident.t -> string -> Js_cmj_format.keyed_cmj_value
(**
   [query_external_id_info id pos env found]
   will raise if not found
*)

val is_pure_module : Lam_module_ident.t -> bool

val get_package_path_from_cmj :
  Lam_module_ident.t -> string * Js_packages_info.t * Ext_js_file_kind.case

(* The second argument is mostly from [runtime] modules
    will change the input [hard_dependencies]
    [populate_required_modules extra hard_dependencies]
    [extra] maybe removed if it is pure and not in [hard_dependencies]
*)
val populate_required_modules :
  Lam_module_ident.Hash_set.t -> Lam_module_ident.Hash_set.t -> unit
