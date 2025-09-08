(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

module Set_string = Ast_extract.Set_string
(** Synced up with module {!Bsb_helper_depfile_gen} *)

type 'a kind = 'a Ml_binary.kind =
  | Ml : Parsetree.structure kind
  | Mli : Parsetree.signature kind

let read_ast_exn (type t) ~fname (_ : t kind) : t =
  let ic = open_in_bin fname in
  let dep_size = input_binary_int ic in
  seek_in ic (pos_in ic + dep_size);
  let sourcefile = input_line ic in
  Location.set_input_name sourcefile;
  let ast = input_value ic in
  close_in ic;
  ast

let magic_sep_char = '\n'

(*
   Reasons that we don't [output_value] the set:
   1. for performance , easy skipping and calcuate the length 
   2. cut dependency, otherwise its type is {!Ast_extract.Set_string.t}
*)
let write_ast (type t) ~(sourcefile : string) ~output (kind : t kind) (pt : t) :
    unit =
  let output_set = Ast_extract.read_parse_and_extract kind pt in
  let buf = Ext_buffer.create 1000 in
  Ext_buffer.add_char buf magic_sep_char;
  Set_string.iter
    (fun s ->
      if s <> "" && s.[0] <> '*' then
        (* filter *predef* *)
        Ext_buffer.add_string_char buf s magic_sep_char)
    output_set;
  let oc = open_out_bin output in
  output_binary_int oc (Ext_buffer.length buf);
  Ext_buffer.output_buffer oc buf;
  output_string oc sourcefile;
  output_char oc '\n';
  output_value oc pt;
  close_out oc
