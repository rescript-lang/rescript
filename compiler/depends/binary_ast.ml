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

type result =
  | Implementation of {
      sourcefile: string;
      dependencies: string list;
      ast: Parsetree.structure;
    }
  | Interface of {
      sourcefile: string;
      dependencies: string list;
      ast: Parsetree.signature;
    }

let dependencies = function
  | Implementation {dependencies; _} | Interface {dependencies; _} ->
    dependencies

let capture_key = Domain.DLS.new_key (fun () -> None)
let lookup_key = Domain.DLS.new_key (fun () -> None)

let with_capture capture action =
  let previous = Domain.DLS.get capture_key in
  Domain.DLS.set capture_key (Some capture);
  Fun.protect action ~finally:(fun () -> Domain.DLS.set capture_key previous)

let with_lookup lookup action =
  let previous = Domain.DLS.get lookup_key in
  Domain.DLS.set lookup_key (Some lookup);
  Fun.protect action ~finally:(fun () -> Domain.DLS.set lookup_key previous)

let read_ast_exn (type t) ~fname (kind : t kind) : t =
  let staged =
    match Domain.DLS.get lookup_key with
    | None -> None
    | Some lookup -> lookup fname
  in
  match (kind, staged) with
  | Ml, Some (Implementation {sourcefile; ast; _}) ->
    Compiler_phase_trace.dependency "dependency.session_ast_lookup" (fun () ->
        Location.set_input_name sourcefile;
        ast)
  | Mli, Some (Interface {sourcefile; ast; _}) ->
    Compiler_phase_trace.dependency "dependency.session_ast_lookup" (fun () ->
        Location.set_input_name sourcefile;
        ast)
  | _, _ ->
    Compiler_phase_trace.dependency "dependency.ast_read_decode" (fun () ->
        let ic = open_in_bin (Compiler_request_state.resolve_path fname) in
        Fun.protect
          (fun () ->
            let dep_size = input_binary_int ic in
            seek_in ic (pos_in ic + dep_size);
            let sourcefile = input_line ic in
            Location.set_input_name sourcefile;
            input_value ic)
          ~finally:(fun () -> close_in_noerr ic))

let magic_sep_char = '\n'

let serialize_result result =
  let sourcefile, dependencies, ast =
    match result with
    | Implementation {sourcefile; dependencies; ast} ->
      (sourcefile, dependencies, Marshal.to_string ast [])
    | Interface {sourcefile; dependencies; ast} ->
      (sourcefile, dependencies, Marshal.to_string ast [])
  in
  let dependency_buffer = Buffer.create 1000 in
  Buffer.add_char dependency_buffer magic_sep_char;
  List.iter
    (fun dependency ->
      Buffer.add_string dependency_buffer dependency;
      Buffer.add_char dependency_buffer magic_sep_char)
    dependencies;
  let dependency_size = Buffer.length dependency_buffer in
  let output = Buffer.create (dependency_size + String.length ast + 128) in
  for shift = 3 downto 0 do
    Buffer.add_char output
      (Char.chr ((dependency_size lsr (shift * 8)) land 255))
  done;
  Buffer.add_buffer output dependency_buffer;
  Buffer.add_string output sourcefile;
  Buffer.add_char output '\n';
  Buffer.add_string output ast;
  Buffer.contents output

(*
   Reasons that we don't [output_value] the set:
   1. for performance , easy skipping and calcuate the length 
   2. cut dependency, otherwise its type is {!Ast_extract.Set_string.t}
*)
let write_ast (type t) ~(sourcefile : string) ~output (kind : t kind) (pt : t) :
    unit =
  let output_set = Ast_extract.read_parse_and_extract kind pt in
  let dependencies =
    Set_string.elements output_set
    |> List.filter (fun s -> s <> "" && s.[0] <> '*')
  in
  let result =
    match kind with
    | Ml -> Implementation {sourcefile; dependencies; ast = pt}
    | Mli -> Interface {sourcefile; dependencies; ast = pt}
  in
  let deferred =
    match Domain.DLS.get capture_key with
    | None -> false
    | Some capture -> capture output result
  in
  if deferred then
    try Unix.unlink (Compiler_request_state.resolve_path output)
    with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  else
    let buf = Ext_buffer.create 1000 in
    Ext_buffer.add_char buf magic_sep_char;
    List.iter
      (fun s -> Ext_buffer.add_string_char buf s magic_sep_char)
      dependencies;
    let oc = open_out_bin (Compiler_request_state.resolve_path output) in
    output_binary_int oc (Ext_buffer.length buf);
    Ext_buffer.output_buffer oc buf;
    output_string oc sourcefile;
    output_char oc '\n';
    output_value oc pt;
    close_out oc
