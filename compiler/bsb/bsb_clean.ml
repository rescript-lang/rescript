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

let ( // ) = Ext_path.combine

let ninja_clean proj_dir =
  try
    let cmd = Bsb_global_paths.vendor_ninja in
    let lib_artifacts_dir = Bsb_config.lib_bs in
    let cwd = proj_dir // lib_artifacts_dir in
    if Sys.file_exists cwd then
      let eid =
        Bsb_unix.run_command_execv {cmd; args = [|cmd; "-t"; "clean"|]; cwd}
      in
      if eid <> 0 then Bsb_log.warn "@{<warning>Failed@}@."
  with e -> Bsb_log.warn "@{<warning>Failed@}: %s @." (Printexc.to_string e)

let clean_bs_garbage proj_dir =
  Bsb_log.info "@{<info>Cleaning:@} in %s@." proj_dir;
  let try_remove x =
    let x = proj_dir // x in
    if Sys.file_exists x then Bsb_unix.remove_dir_recursive x
  in
  try
    Bsb_parse_sources.clean_re_js proj_dir;
    (* clean re.js files*)
    ninja_clean proj_dir;
    Ext_list.iter Bsb_config.all_lib_artifacts try_remove
  with e ->
    Bsb_log.warn "@{<warning>Failed@} to clean due to %s" (Printexc.to_string e)

let clean_bs_deps proj_dir =
  let _, _ = Bsb_config_parse.deps_from_bsconfig () in
  let queue = Bsb_build_util.walk_all_deps proj_dir in
  Queue.iter
    (fun (pkg_cxt : Bsb_build_util.package_context) ->
      (* whether top or not always do the cleaning *)
      clean_bs_garbage pkg_cxt.proj_dir)
    queue

let clean_self proj_dir = clean_bs_garbage proj_dir
