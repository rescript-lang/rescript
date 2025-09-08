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

type cmi_infos = {
  cmi_name: string;
  cmi_sign: Types.signature_item list;
  cmi_crcs: (string * Digest.t option) list;
  cmi_flags: pers_flags list;
}

(* write the magic + the cmi information *)
val output_cmi : string -> out_channel -> cmi_infos -> Digest.t

val create_cmi : ?check_exists:unit -> string -> cmi_infos -> Digest.t

(* read the cmi information (the magic is supposed to have already been read) *)
val input_cmi : in_channel -> cmi_infos

(* read a cmi from a filename, checking the magic *)
val read_cmi : string -> cmi_infos

(* Error report *)

type error =
  | Not_an_interface of string
  | Wrong_version_interface of string * string
  | Corrupted_interface of string

exception Error of error

open Format

val report_error : formatter -> error -> unit
