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

type 'a kind = 'a Ml_binary.kind =
  | Ml : Parsetree.structure kind
  | Mli : Parsetree.signature kind

let read_ast_exn (type t) ~fname (_ : t kind) : t =
  let path = Compiler_request_state.resolve_path fname in
  let contents =
    match Build_file_store.read path with
    | Some contents -> contents
    | None -> raise (Sys_error (path ^ ": No such file or directory"))
  in
  if String.length contents < 4 then raise End_of_file;
  let source_start = 4 + Int32.to_int (String.get_int32_be contents 0) in
  let source_end =
    match String.index_from_opt contents source_start '\n' with
    | Some index -> index
    | None -> raise End_of_file
  in
  Location.set_input_name
    (String.sub contents source_start (source_end - source_start));
  Marshal.from_string contents (source_end + 1)

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
  let length = Bytes.create 4 in
  Bytes.set_int32_be length 0 (Int32.of_int (Ext_buffer.length buf));
  Build_file_store.write
    (Compiler_request_state.resolve_path output)
    (String.concat ""
       [
         Bytes.unsafe_to_string length;
         Ext_buffer.contents buf;
         sourcefile;
         "\n";
         Marshal.to_string pt [];
       ])
