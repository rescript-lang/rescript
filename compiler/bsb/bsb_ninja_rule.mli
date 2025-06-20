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

type t
(** The complexity comes from the fact that we allow custom rules which could
    conflict with our custom built-in rules
*)

val get_name : t -> out_channel -> string

(***********************************************************)

type builtin = {
  build_ast_from_re: t;
  (* platform dependent, on Win32,
      invoking cmd.exe
  *)
  copy_resources: t;
  (* Rules below all need restat *)
  build_bin_deps: t;
  build_bin_deps_dev: t;
  mj: t;
  mj_dev: t;
  mij: t;
  mij_dev: t;
  mi: t;
  mi_dev: t;
  build_package: t;
  customs: t Map_string.t;
}
(** A list of existing rules *)

(***********************************************************)

(** rules are generally composed of built-in rules and customized rules, there are two design choices:
    1. respect custom rules with the same name, then we need adjust our built-in 
    rules dynamically in case the conflict.
    2. respect our built-in rules, then we only need re-load custom rules for each rescript.json
*)

type command = string

(* Since now we generate ninja files per rescript.json in a single process,
    we must make sure it is re-entrant
*)
val make_custom_rules :
  gentype_config:Bsb_config_types.gentype_config ->
  has_postbuild:string option ->
  pp_file:string option ->
  jsx:Bsb_jsx.t ->
  digest:string ->
  package_specs:Bsb_package_specs.t ->
  namespace:string option ->
  package_name:string ->
  warnings:string ->
  ppx_files:Bsb_config_types.ppx list ->
  bsc_flags:string ->
  dpkg_incls:string ->
  lib_incls:string ->
  dev_incls:string ->
  bs_dependencies:Bsb_config_types.dependencies ->
  bs_dev_dependencies:Bsb_config_types.dependencies ->
  command Map_string.t ->
  builtin
