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

let write_file fname digest contents =
  let oc = open_out_bin fname in
  Digest.output oc digest;
  output_char oc '\n';
  Ext_buffer.output_buffer oc contents;
  close_out oc

(*
   TODO:
   sort filegroupts to ensure deterministic behavior

   if [.bsbuild] is not changed
   [.mlmap] does not need to be changed too

*)
let output ~dir (namespace : string) (file_groups : Bsb_file_groups.file_groups)
    =
  let fname = namespace ^ Literals.suffix_mlmap in
  let buf = Ext_buffer.create 10000 in
  Ext_list.iter file_groups (fun x ->
      Map_string.iter x.sources (fun k _ ->
          Ext_buffer.add_string_char buf k '\n'));
  (* let contents = Buffer.contents buf in    *)
  let digest = Ext_buffer.digest buf in
  let fname = dir // fname in
  if Sys.file_exists fname then (
    let ic = open_in_bin fname in
    let old_digest = really_input_string ic Ext_digest.length in
    close_in ic;
    if old_digest <> digest then write_file fname digest buf)
  else write_file fname digest buf
