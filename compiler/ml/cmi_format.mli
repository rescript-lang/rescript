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

val create_cmi : ?check_exists:unit -> string -> cmi_infos -> Digest.t

(* read the cmi information (the magic is supposed to have already been read) *)
val input_cmi : in_channel -> cmi_infos

(* read a cmi from a filename, checking the magic *)
val read_cmi : string -> cmi_infos
val read_cmi_channel : string -> in_channel -> cmi_infos

val read_cmi_contents : string -> string -> cmi_infos
(** [read_cmi_contents filename contents] reads an interface from the
    contents of [filename]. *)

val read_crcs_and_flags :
  string -> string -> (string * Digest.t option) list * pers_flags list
(** [read_crcs_and_flags filename contents] reads the CRCs and flags of the
    interface in [contents], without reading its signature. The first CRC is
    the interface's own; the others are those of its imports. *)
(* Parse an already opened CMI. The caller retains ownership of the channel. *)

(* Error report *)

type error =
  | Not_an_interface of string
  | Wrong_version_interface of string * string
  | Corrupted_interface of string

exception Error of error

open Format

val report_error : formatter -> error -> unit
