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

let magic_sep_char = '\n'

type header = {source_start: int; source_end: int}

let read_header ?length contents =
  let length = Option.value length ~default:(String.length contents) in
  if length < 4 then None
  else
    let source_start = 4 + Int32.to_int (String.get_int32_be contents 0) in
    if source_start < 4 || source_start > length then None
    else
      let rec source_end index =
        if index >= length then None
        else if contents.[index] = magic_sep_char then
          Some {source_start; source_end = index}
        else source_end (index + 1)
      in
      source_end source_start

let header_dependencies contents header =
  String.sub contents 4 (header.source_start - 4)
  |> String.split_on_char magic_sep_char
  |> List.filter (fun name -> name <> "")

let header_source contents header =
  String.sub contents header.source_start
    (header.source_end - header.source_start)

let read_ast_exn (type t) ~fname (_ : t kind) : t =
  let contents = Ext_io.load_file fname in
  match read_header contents with
  | Some header ->
    Location.set_input_name (header_source contents header);
    Ext_io.unmarshal contents (header.source_end + 1)
  | None -> raise End_of_file

(*
   Reasons that we don't [output_value] the set:
   1. for performance , easy skipping and calcuate the length 
   2. cut dependency, otherwise its type is {!Ast_extract.Set_string.t}
*)
let write_ast (type t) ~(sourcefile : string) ~output (kind : t kind) (pt : t) :
    unit =
  Compiler_request_state.before_effect ();
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
