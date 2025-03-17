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

[@@@warning "+9"]

(* float_of_string_opt *)
external hexstring_of_float : float -> int -> char -> string
  = "caml_hexstring_of_float"

let hex_of_float f = hexstring_of_float f (-1) '-'

(* This should not lose any preicision *)
(* let id (f : float) =
    float_of_string (hex_of_float f) = f
*)

type check_result =
  | Good
  | Bsb_file_corrupted
  | Bsb_file_not_exist  (** We assume that it is a clean repo *)
  | Bsb_source_directory_changed
  | Bsb_bsc_version_mismatch
  | Bsb_forced
  | Bsb_package_kind_inconsistent
  | Bsb_regenerate_required
  | Other of string

let pp_check_result fmt (check_resoult : check_result) =
  Format.pp_print_string fmt
    (match check_resoult with
    | Good -> "OK"
    | Bsb_file_corrupted -> "Stored data corrupted"
    | Bsb_file_not_exist -> "Dependencies information missing"
    | Bsb_source_directory_changed -> "Bsb source directory changed"
    | Bsb_bsc_version_mismatch -> "Bsc or bsb version mismatch"
    | Bsb_forced -> "Bsb forced rebuild"
    | Bsb_package_kind_inconsistent -> "The package was built in different mode"
    | Bsb_regenerate_required -> "Bsb need regenerate build.ninja"
    | Other s -> s)

let rec check_aux cwd (xs : string list) =
  match xs with
  | [] -> Good
  | "===" :: rest -> check_global_atime rest
  | item :: rest -> (
    match Ext_string.split item '\t' with
    | [file; stamp] ->
      let stamp = float_of_string stamp in
      let cur_file = Filename.concat cwd file in
      let stat = Unix.stat cur_file in
      if stat.st_mtime <= stamp then check_aux cwd rest else Other cur_file
    | _ -> Bsb_file_corrupted)

and check_global_atime rest =
  match rest with
  | [] -> Good
  | item :: rest -> (
    match Ext_string.split item '\t' with
    | [file; stamp] ->
      let stamp = float_of_string stamp in
      let cur_file = file in
      let stat = Unix.stat cur_file in
      if stat.st_atime <= stamp then check_global_atime rest else Other cur_file
    | _ -> Bsb_file_corrupted)

(* TODO: for such small data structure, maybe text format is better *)

let record_global_atime buf name =
  let stamp = (Unix.stat name).st_atime in
  Ext_buffer.add_string_char buf name '\t';
  Ext_buffer.add_string_char buf (hex_of_float stamp) '\n'

let record ~(package_kind : Bsb_package_kind.t) ~per_proj_dir ~file
    ~(config : Bsb_config_types.t) ~(warn_as_error : string option)
    (file_or_dirs : string list) : unit =
  let buf = Ext_buffer.create 1_000 in
  Ext_buffer.add_string_char buf Bs_version.version '\n';
  Ext_buffer.add_string_char buf per_proj_dir '\n';
  Ext_buffer.add_string_char buf
    (Bsb_package_kind.encode_no_nl package_kind)
    '\n';
  Ext_buffer.add_string_char buf
    (match warn_as_error with
    | Some s -> s
    | None -> "0")
    '\n';
  Ext_list.iter file_or_dirs (fun f ->
      Ext_buffer.add_string_char buf f '\t';
      Ext_buffer.add_string_char buf
        (hex_of_float (Unix.stat (Filename.concat per_proj_dir f)).st_mtime)
        '\n');
  Ext_buffer.add_string buf "===\n";
  record_global_atime buf Sys.executable_name;
  Ext_list.iter config.ppx_files (fun {name; args = _} ->
      try record_global_atime buf name
      with _ ->
        (* record the ppx files as a best effort *)
        ());
  let oc = open_out_bin file in
  Ext_buffer.output_buffer oc buf;
  close_out oc

(** check time stamp for all files
    TODO: those checks system call can be saved later
    Return a reason
    Even forced, we still need walk through a little
    bit in case we found a different version of compiler
*)
let check ~(package_kind : Bsb_package_kind.t) ~(per_proj_dir : string) ~forced
    ~(warn_as_error : string option) ~file : check_result =
  match open_in_bin file with
  (* Windows binary mode*)
  | exception _ -> Bsb_file_not_exist
  | ic -> (
    match List.rev (Ext_io.rev_lines_of_chann ic) with
    | exception _ -> Bsb_file_corrupted
    | version :: source_directory :: package_kind_str :: previous_warn_as_error
      :: dir_or_files -> (
      let warn_as_error_changed =
        match warn_as_error with
        | None -> previous_warn_as_error <> "0"
        | Some current -> current <> previous_warn_as_error
      in

      if version <> Bs_version.version then Bsb_bsc_version_mismatch
      else if per_proj_dir <> source_directory then Bsb_source_directory_changed
      else if forced then Bsb_forced (* No need walk through *)
      else if Bsb_package_kind.encode_no_nl package_kind <> package_kind_str
      then Bsb_package_kind_inconsistent
      else if warn_as_error_changed then Bsb_regenerate_required
      else
        try check_aux per_proj_dir dir_or_files
        with e ->
          Bsb_log.info "@{<info>Stat miss %s@}@." (Printexc.to_string e);
          Bsb_file_not_exist)
    | _ -> Bsb_file_corrupted)
