(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type pers_flags = Deprecated of string

type error =
  | Not_an_interface of string
  | Wrong_version_interface of string * string
  | Corrupted_interface of string

exception Error of error

type cmi_infos = {
  cmi_name: string;
  cmi_sign: Types.signature_item list;
  cmi_crcs: (string * Digest.t option) list;
  cmi_flags: pers_flags list;
}

(* The signature of a file is the boxed graph of [Types_boxed]; it is imported
   into [store], by default the current store. *)
let import_sign ?store (sign : Types_boxed.signature) =
  Type_arena.Import.run ?store (fun c -> Type_arena.Import.signature c sign)

let input_cmi ?store ic =
  let name, (sign : Types_boxed.signature) = input_value ic in
  let sign = import_sign ?store sign in
  let crcs = input_value ic in
  let flags = input_value ic in
  {cmi_name = name; cmi_sign = sign; cmi_crcs = crcs; cmi_flags = flags}

(* Earlier compilers wrote OCaml-style magic numbers *)
let legacy_magic_prefix = "Caml1999I"

let check_magic_number filename buffer =
  if buffer <> Config.cmi_magic_number then
    let pre_len = String.length Config.cmi_magic_number - 3 in
    if
      String.sub buffer 0 pre_len = String.sub Config.cmi_magic_number 0 pre_len
    then
      let msg =
        if buffer < Config.cmi_magic_number then "an older" else "a newer"
      in
      raise (Error (Wrong_version_interface (filename, msg)))
    else if String.starts_with ~prefix:legacy_magic_prefix buffer then
      raise (Error (Wrong_version_interface (filename, "an older")))
    else raise (Error (Not_an_interface filename))

let read_cmi_channel ?store filename ic =
  try
    check_magic_number filename
      (really_input_string ic (String.length Config.cmi_magic_number));
    input_cmi ?store ic
  with End_of_file | Failure _ -> raise (Error (Corrupted_interface filename))

(* The same as [read_cmi_channel] on a channel holding [contents]. *)
let read_cmi_parts filename contents =
  let magic_length = String.length Config.cmi_magic_number in
  if String.length contents < magic_length then
    raise (Error (Corrupted_interface filename));
  check_magic_number filename (String.sub contents 0 magic_length);
  try
    let next offset =
      offset + Marshal.total_size (Bytes.unsafe_of_string contents) offset
    in
    let input offset = (Marshal.from_string contents offset, next offset) in
    let crcs_offset = next magic_length in
    let crcs, flags_offset = input crcs_offset in
    let flags, _ = input flags_offset in
    (magic_length, crcs, flags)
  with Failure _ | Invalid_argument _ ->
    raise (Error (Corrupted_interface filename))

let read_cmi_contents ?store filename contents =
  let sign_offset, crcs, flags = read_cmi_parts filename contents in
  let name, (sign : Types_boxed.signature) =
    try Marshal.from_string contents sign_offset
    with Failure _ | Invalid_argument _ ->
      raise (Error (Corrupted_interface filename))
  in
  {
    cmi_name = name;
    cmi_sign = import_sign ?store sign;
    cmi_crcs = crcs;
    cmi_flags = flags;
  }

let read_cmi filename =
  match
    Build_file_store.read (Compiler_request_state.resolve_path filename)
  with
  | Some contents -> read_cmi_contents filename contents
  | None ->
    raise
      (Sys_error
         (Compiler_request_state.resolve_path filename
         ^ ": No such file or directory"))

(* Writes the .cmi file, keeping an existing one with the same contents
   when [check_exists] is given *)
let create_cmi ?check_exists filename (cmi : cmi_infos) =
  (* beware: the provided signature must have been substituted for saving *)
  let sign =
    Type_arena.Export.run (fun c -> Type_arena.Export.signature c cmi.cmi_sign)
  in
  let content =
    Config.cmi_magic_number ^ Marshal.to_string (cmi.cmi_name, sign) []
    (* checkout [output_value] in {!Pervasives} module *)
  in
  let crc = Digest.string content in
  (* The existing file's signature is not needed, only its crcs and flags. *)
  let old_parts =
    if check_exists = None then None
    else
      Build_file_store.read (Compiler_request_state.resolve_path filename)
      |> Option.map (fun contents ->
          let _, crcs, flags = read_cmi_parts filename contents in
          (crcs, flags))
  in
  match old_parts with
  | Some ((old_name, Some old_crc) :: rest, cmi_flags)
    when cmi.cmi_name = old_name && crc = old_crc && cmi.cmi_crcs = rest
         && cmi_flags = cmi.cmi_flags ->
    crc
  | _ ->
    let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
    Build_file_store.write
      (Compiler_request_state.resolve_path filename)
      (String.concat ""
         [
           content; Marshal.to_string crcs []; Marshal.to_string cmi.cmi_flags [];
         ]);
    crc

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename ->
    fprintf ppf "%a@ is not a compiled interface" Location.print_filename
      filename
  | Wrong_version_interface (filename, older_newer) ->
    fprintf ppf
      "%a@ is not a compiled interface for this version of ReScript.@.It seems \
       to be for %s version of ReScript."
      Location.print_filename filename older_newer
  | Corrupted_interface filename ->
    fprintf ppf "Corrupted compiled interface@ %a" Location.print_filename
      filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
