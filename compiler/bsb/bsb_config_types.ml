(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type dependency = {package_name: Bsb_pkg_types.t; package_install_path: string}

type dependencies = dependency list

type gentype_config = bool
type command = string
type ppx = {name: string; args: string list}

type t = {
  package_name: string;
  (* [captial-package] *)
  namespace: string option;
  (* CapitalPackage *)
  external_includes: string list;
  bsc_flags: string list;
  ppx_files: ppx list;
  pp_file: string option;
  bs_dependencies: dependencies;
  bs_dev_dependencies: dependencies;
  warning: Bsb_warning.t;
  (*TODO: maybe we should always resolve rescript
    so that we can calculate correct relative path in
    [.merlin]
  *)
  js_post_build_cmd: string option;
  package_specs: Bsb_package_specs.t;
  file_groups: Bsb_file_groups.t;
  files_to_install: Bsb_db.module_info Queue.t;
  jsx: Bsb_jsx.t;
  (* whether apply PPX transform or not*)
  generators: command Map_string.t;
  cut_generators: bool;
  (* note when used as a dev mode, we will always ignore it *)
  gentype_config: gentype_config;
  filename: string;
}
