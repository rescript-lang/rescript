(* Copyright (C) 2015 - Hongbo Zhang, Authors of ReScript
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
 * LGPL version 3 (or the corresponding section of a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let temp_ppx_file () =
  Filename.temp_file "ppx" (Filename.basename (Location.get_input_name ()))

let write_ast fn (ast0 : Ml_binary.ast0) =
  let source =
    Compiler_phase_trace.section "ppx.serialize" (fun () ->
        Marshal.to_string (Location.get_input_name () : string) [])
  in
  let ast =
    Compiler_phase_trace.section "ppx.serialize" (fun () ->
        match ast0 with
        | Ml_binary.Impl tree ->
          Marshal.to_string (tree : Parsetree0.structure) []
        | Ml_binary.Intf tree ->
          Marshal.to_string (tree : Parsetree0.signature) [])
  in
  Compiler_phase_trace.section "ppx.write" (fun () ->
      let channel = open_out_bin fn in
      Fun.protect
        (fun () ->
          output_string channel (Ml_binary.magic_of_ast0 ast0);
          output_string channel source;
          output_string channel ast)
        ~finally:(fun () -> close_out_noerr channel))

let apply_rewriter kind fn_in ppx =
  let magic = Ml_binary.magic_of_kind kind in
  let fn_out = temp_ppx_file () in
  let command =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok =
    Compiler_phase_trace.section "ppx.execute" (fun () ->
        Ccomp.command command = 0)
  in
  if not ok then Cmd_ast_exception.cannot_run command;
  if not (Sys.file_exists fn_out) then Cmd_ast_exception.cannot_run command;
  let buffer =
    Compiler_phase_trace.section "ppx.read" (fun () ->
        let channel = open_in_bin fn_out in
        Fun.protect
          (fun () ->
            try really_input_string channel (String.length magic)
            with End_of_file -> "")
          ~finally:(fun () -> close_in_noerr channel))
  in
  if buffer <> magic then Cmd_ast_exception.wrong_magic buffer;
  fn_out

let read_ast (type a) (kind : a Ml_binary.kind) fn : Ml_binary.ast0 =
  let magic = Ml_binary.magic_of_kind kind in
  let source, encoded_ast =
    Compiler_phase_trace.section "ppx.read" (fun () ->
        let channel = open_in_bin fn in
        Fun.protect
          (fun () ->
            let buffer = really_input_string channel (String.length magic) in
            assert (buffer = magic);
            let source = (input_value channel : string) in
            let bytes = in_channel_length channel - pos_in channel in
            (source, really_input_string channel bytes))
          ~finally:(fun () -> close_in_noerr channel))
  in
  Location.set_input_name source;
  Compiler_phase_trace.section "ppx.deserialize" (fun () ->
      match kind with
      | Ml_binary.Ml ->
        Ml_binary.Impl
          (Marshal.from_string encoded_ast 0 : Parsetree0.structure)
      | Ml_binary.Mli ->
        Ml_binary.Intf
          (Marshal.from_string encoded_ast 0 : Parsetree0.signature))

(* [-ppx1 -ppx2 -ppx3] is stored in reverse order. *)
let rewrite (type a) (kind : a Ml_binary.kind) ppxs (ast : a) : a =
  let fn_in = temp_ppx_file () in
  let ast0 =
    Compiler_phase_trace.section "ppx.convert.to0" (fun () ->
        Ml_binary.to_ast0 kind ast)
  in
  write_ast fn_in ast0;
  let temp_files =
    List.fold_right
      (fun ppx fns ->
        match fns with
        | [] -> assert false
        | fn_in :: _ -> apply_rewriter kind fn_in ppx :: fns)
      ppxs [fn_in]
  in
  match temp_files with
  | last_fn :: _ ->
    let output = read_ast kind last_fn in
    Ext_list.iter temp_files Misc.remove_file;
    Compiler_phase_trace.section "ppx.convert.from0" (fun () : a ->
        match kind with
        | Ml_binary.Ml -> Ml_binary.ast0_to_structure output
        | Ml_binary.Mli -> Ml_binary.ast0_to_signature output)
  | [] -> assert false
