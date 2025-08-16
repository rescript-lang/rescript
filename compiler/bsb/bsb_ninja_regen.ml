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

let bsdeps = ".bsdeps"

let ( // ) = Ext_path.combine

(** Regenerate ninja file by need based on [.bsdeps]
    return None if we dont need regenerate
    otherwise return Some info
*)
let regenerate_ninja ~(package_kind : Bsb_package_kind.t) ~forced ~per_proj_dir
    ~warn_legacy_config ~warn_as_error : Bsb_config_types.t option =
  let lib_artifacts_dir = Bsb_config.lib_bs in
  let lib_bs_dir = per_proj_dir // lib_artifacts_dir in
  let output_deps = lib_bs_dir // bsdeps in
  let check_result =
    Bsb_ninja_check.check ~package_kind ~per_proj_dir ~forced ~warn_as_error
      ~file:output_deps
  in
  let config_filename, config_json =
    Bsb_config_load.load_json ~per_proj_dir ~warn_legacy_config
  in
  match check_result with
  | Good -> None (* Fast path, no need regenerate ninja *)
  | Bsb_forced | Bsb_bsc_version_mismatch | Bsb_package_kind_inconsistent
  | Bsb_file_corrupted | Bsb_file_not_exist | Bsb_source_directory_changed
  | Bsb_regenerate_required | Other _ ->
    Bsb_log.info "@{<info>BSB check@} build spec : %a @."
      Bsb_ninja_check.pp_check_result check_result;
    if check_result = Bsb_bsc_version_mismatch then (
      Bsb_log.warn "@{<info>Different compiler version@}: clean current repo@.";
      Bsb_clean.clean_bs_deps per_proj_dir;
      Bsb_clean.clean_self per_proj_dir);

    let config : Bsb_config_types.t =
      Bsb_config_parse.interpret_json ~filename:config_filename
        ~json:config_json ~package_kind ~per_proj_dir
    in

    let warning =
      match config.warning with
      | None -> (
        match warn_as_error with
        | Some e ->
          Some {Bsb_warning.number = Some e; error = Warn_error_number e}
        | None -> None)
      | Some {error} as t -> (
        match (warn_as_error, error) with
        | Some error_str, Warn_error_false ->
          Some {number = Some error_str; error = Warn_error_number error_str}
        | Some error_str, Warn_error_number prev ->
          let new_error = prev ^ error_str in
          Some {number = Some new_error; error = Warn_error_number new_error}
        | _ -> t)
    in

    let config = {config with warning} in
    (* create directory, lib/bs, lib/js, lib/es6 etc *)
    Bsb_build_util.mkp lib_bs_dir;
    Bsb_package_specs.list_dirs_by config.package_specs (fun x ->
        let dir = per_proj_dir // x in
        (*Unix.EEXIST error*)
        if not (Sys.file_exists dir) then Unix.mkdir dir 0o777);
    (match package_kind with
    | Toplevel ->
      Bsb_watcher_gen.generate_sourcedirs_meta
        ~name:(lib_bs_dir // Literals.sourcedirs_meta)
        config.file_groups
    | Dependency _ -> ());

    Bsb_ninja_gen.output_ninja_and_namespace_map ~per_proj_dir ~package_kind
      config;
    (* PR2184: we still need record empty dir
        since it may add files in the future *)
    Bsb_ninja_check.record ~package_kind ~per_proj_dir ~config ~warn_as_error
      ~file:output_deps
      (config.filename :: config.file_groups.globbed_dirs);
    Some config
