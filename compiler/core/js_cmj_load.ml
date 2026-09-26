(* Copyright (C) Hongbo Zhang, Authors of ReScript
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

(* strategy:
   If not installed, use the distributed [cmj] files,
   make sure that the distributed files are platform independent
*)

let session_lookup_key = Domain.DLS.new_key (fun () -> None)

let with_session_lookup lookup action =
  let previous = Domain.DLS.get session_lookup_key in
  Domain.DLS.set session_lookup_key (Some lookup);
  Fun.protect action ~finally:(fun () ->
      Domain.DLS.set session_lookup_key previous)

let load_unit_with_file unit_name : Js_cmj_format.cmj_load_info =
  let file = unit_name ^ Literals.suffix_cmj in
  let selected =
    match Domain.DLS.get session_lookup_key with
    | None ->
      Option.map (fun filename -> (filename, None)) (Config_util.find_opt file)
    | Some lookup ->
      let lower_file = Ext_string.uncapitalize_ascii file in
      let rec find = function
        | [] -> None
        | directory :: rest -> (
          let lower = Filename.concat directory lower_file in
          let exact = Filename.concat directory file in
          match lookup unit_name lower with
          | Some table -> Some (lower, Some table)
          | None -> (
            match lookup unit_name exact with
            | Some table ->
              if
                Compiler_request_state.is_regular_file lower
                && Compiler_request_state.has_exact_directory_entry lower
              then Some (lower, None)
              else Some (exact, Some table)
            | None ->
              if Compiler_request_state.is_regular_file lower then
                Some (lower, None)
              else if Compiler_request_state.is_regular_file exact then
                Some (exact, None)
              else find rest))
      in
      find (Config.get_load_path ())
  in
  match selected with
  | Some (f, session_table) ->
    {
      package_path =
        (* hacking relying on the convention of pkg/lib/ocaml/xx.cmj*)
        Filename.dirname (Filename.dirname (Filename.dirname f));
      cmj_table =
        (match session_table with
        | Some table ->
          Compiler_phase_trace.dependency "dependency.session_cmj_lookup"
            (fun () -> table)
        | None -> Js_cmj_format.from_file f);
    }
  | None -> Bs_exception.error (Cmj_not_found unit_name)

(* we can disable loading from file for troubleshooting
   Note in dev mode we still allow loading from file is to
   make the dev build still function correct
*)
let load_unit = ref load_unit_with_file
