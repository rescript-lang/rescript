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

(* Note: some of the functions here should go to Ast_mapper instead,
   which would encapsulate the "binary AST" protocol. *)

let write_ast fn (ast0 : Ml_binary.ast0) =
  let oc = open_out_bin fn in
  output_string oc (Ml_binary.magic_of_ast0 ast0);
  output_value oc (Location.get_input_name () : string);
  (match ast0 with
  | Ml_binary.Impl ast -> output_value oc (ast : Parsetree0.structure)
  | Ml_binary.Intf ast -> output_value oc (ast : Parsetree0.signature));
  close_out oc

(* Absolute, because the PPX runs in the request's directory while a relative
   temporary directory names one below the process's. *)
let temp_ppx_file () =
  let path =
    Filename.temp_file "ppx" (Filename.basename (Location.get_input_name ()))
  in
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let apply_rewriter kind fn_in ppx fn_out =
  let magic = Ml_binary.magic_of_kind kind in
  let comm =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  if not ok then Cmd_ast_exception.cannot_run comm;
  if not (Sys.file_exists fn_out) then Cmd_ast_exception.cannot_run comm;
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> ""
  in
  close_in ic;
  if buffer <> magic then Cmd_ast_exception.wrong_magic buffer

(* This is a fatal error, no need to protect it *)
let read_ast (type a) (kind : a Ml_binary.kind) fn : Ml_binary.ast0 =
  let ic = open_in_bin fn in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let magic = Ml_binary.magic_of_kind kind in
      let buffer = really_input_string ic (String.length magic) in
      assert (buffer = magic);
      (* already checked by apply_rewriter *)
      Location.set_input_name @@ (input_value ic : string);
      match kind with
      | Ml_binary.Ml -> Ml_binary.Impl (input_value ic : Parsetree0.structure)
      | Ml_binary.Mli -> Ml_binary.Intf (input_value ic : Parsetree0.signature))

(** [ppxs] are a stack, 
    [-ppx1 -ppx2  -ppx3]
    are stored as [-ppx3; -ppx2; -ppx1]
    [fold_right] happens to process the first one *)
let rewrite kind ppxs ast =
  (* Remove the temporary files also when a PPX fails or the build is
     interrupted, since the compiler can run inside a long-lived process. *)
  let temp_files = ref [] in
  let new_temp_file () =
    let fn = temp_ppx_file () in
    temp_files := fn :: !temp_files;
    fn
  in
  Fun.protect
    ~finally:(fun () -> Ext_list.iter !temp_files Misc.remove_file)
    (fun () ->
      let fn_in = new_temp_file () in
      write_ast fn_in (Ml_binary.to_ast0 kind ast);
      let last_fn =
        List.fold_right
          (fun ppx fn_in ->
            let fn_out = new_temp_file () in
            apply_rewriter kind fn_in ppx fn_out;
            fn_out)
          ppxs fn_in
      in
      read_ast kind last_fn)

let apply_rewriters_str ~tool_name ast =
  match !((Clflags.current ()).all_ppx) with
  | [] ->
    if !((Js_config.current ()).test_ast_conversion) then
      Ml_binary.ast0_roundtrip Ml_binary.Ml ast
    else ast
  | ppxs ->
    ast
    |> Ast_mapper.add_ppx_context_str ~tool_name
    |> rewrite Ml ppxs |> Ml_binary.ast0_to_structure
    |> Ast_mapper.drop_ppx_context_str

let apply_rewriters_sig ~tool_name ast =
  match !((Clflags.current ()).all_ppx) with
  | [] ->
    if !((Js_config.current ()).test_ast_conversion) then
      Ml_binary.ast0_roundtrip Ml_binary.Mli ast
    else ast
  | ppxs ->
    ast
    |> Ast_mapper.add_ppx_context_sig ~tool_name
    |> rewrite Mli ppxs |> Ml_binary.ast0_to_signature
    |> Ast_mapper.drop_ppx_context_sig

let apply_rewriters ~tool_name (type a) (kind : a Ml_binary.kind) (ast : a) : a
    =
  match kind with
  | Ml_binary.Ml -> apply_rewriters_str ~tool_name ast
  | Ml_binary.Mli -> apply_rewriters_sig ~tool_name ast
