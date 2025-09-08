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

let input_cmi ic =
  let name, sign = input_value ic in
  let crcs = input_value ic in
  let flags = input_value ic in
  {cmi_name = name; cmi_sign = sign; cmi_crcs = crcs; cmi_flags = flags}

let read_cmi filename =
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then (
      close_in ic;
      let pre_len = String.length Config.cmi_magic_number - 3 in
      if
        String.sub buffer 0 pre_len
        = String.sub Config.cmi_magic_number 0 pre_len
      then
        let msg =
          if buffer < Config.cmi_magic_number then "an older" else "a newer"
        in
        raise (Error (Wrong_version_interface (filename, msg)))
      else raise (Error (Not_an_interface filename)));
    let cmi = input_cmi ic in
    close_in ic;
    cmi
  with
  | End_of_file | Failure _ ->
    close_in ic;
    raise (Error (Corrupted_interface filename))
  | Error e ->
    close_in ic;
    raise (Error e)

let output_cmi filename oc cmi =
  (* beware: the provided signature must have been substituted for saving *)
  output_string oc Config.cmi_magic_number;
  output_value oc (cmi.cmi_name, cmi.cmi_sign);
  flush oc;
  let crc = Digest.file filename in
  let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
  output_value oc crcs;
  output_value oc cmi.cmi_flags;
  crc

(* This function is also called by [save_cmt] as cmi_format is subset of
       cmt_format, so dont close the channel yet
*)
let create_cmi ?check_exists filename (cmi : cmi_infos) =
  (* beware: the provided signature must have been substituted for saving *)
  let content =
    Config.cmi_magic_number ^ Marshal.to_string (cmi.cmi_name, cmi.cmi_sign) []
    (* checkout [output_value] in {!Pervasives} module *)
  in
  let crc = Digest.string content in
  let cmi_infos =
    if check_exists <> None && Sys.file_exists filename then
      Some (read_cmi filename)
    else None
  in
  match cmi_infos with
  | Some
      {
        cmi_name = _;
        cmi_sign = _;
        cmi_crcs = (old_name, Some old_crc) :: rest;
        cmi_flags;
      }
  (* TODO: design the cmi format so that we don't need read the whole cmi *)
    when cmi.cmi_name = old_name && crc = old_crc && cmi.cmi_crcs = rest
         && cmi_flags = cmi.cmi_flags ->
    crc
  | _ ->
    let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
    let oc = open_out_bin filename in
    output_string oc content;
    output_value oc crcs;
    output_value oc cmi.cmi_flags;
    close_out oc;
    crc

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename ->
    fprintf ppf "%a@ is not a compiled interface" Location.print_filename
      filename
  | Wrong_version_interface (filename, older_newer) ->
    fprintf ppf
      "%a@ is not a compiled interface for this version of OCaml.@.It seems to \
       be for %s version of OCaml."
      Location.print_filename filename older_newer
  | Corrupted_interface filename ->
    fprintf ppf "Corrupted compiled interface@ %a" Location.print_filename
      filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
