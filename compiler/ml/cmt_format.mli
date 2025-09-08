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

(** cmt and cmti files format. *)

(** The layout of a cmt file is as follows:
      <cmt> := \{<cmi>\} <cmt magic> \{cmt infos\} \{<source info>\}
    where <cmi> is the cmi file format:
      <cmi> := <cmi magic> <cmi info>.
    More precisely, the optional <cmi> part must be present if and only if
    the file is:
    - a cmti, or
    - a cmt, for a ml file which has no corresponding mli (hence no
    corresponding cmti).

    Thus, we provide a common reading function for cmi and cmt(i)
    files which returns an option for each of the three parts: cmi
    info, cmt info, source info. *)

open Typedtree

type binary_annots =
  | Packed of Types.signature * string list
  | Implementation of structure
  | Interface of signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array

and binary_part =
  | Partial_structure of structure
  | Partial_structure_item of structure_item
  | Partial_expression of expression
  | Partial_pattern of pattern
  | Partial_class_expr of unit
  | Partial_signature of signature
  | Partial_signature_item of signature_item
  | Partial_module_type of module_type

type cmt_infos = {
  cmt_modname: string;
  cmt_annots: binary_annots;
  cmt_value_dependencies:
    (Types.value_description * Types.value_description) list;
  cmt_comments: (string * Location.t) list;
  cmt_args: string array;
  cmt_sourcefile: string option;
  cmt_builddir: string;
  cmt_loadpath: string list;
  cmt_source_digest: string option;
  cmt_initial_env: Env.t;
  cmt_imports: (string * Digest.t option) list;
  cmt_interface_digest: Digest.t option;
  cmt_use_summaries: bool;
}

type error = Not_a_typedtree of string

exception Error of error

val read : string -> Cmi_format.cmi_infos option * cmt_infos option
(** [read filename] opens filename, and extract both the cmi_infos, if
    it exists, and the cmt_infos, if it exists. Thus, it can be used
    with .cmi, .cmt and .cmti files.

    .cmti files always contain a cmi_infos at the beginning. .cmt files
    only contain a cmi_infos at the beginning if there is no associated
    .cmti file.
*)

val read_cmt : string -> cmt_infos
val read_cmi : string -> Cmi_format.cmi_infos

val save_cmt :
  string ->
  (* filename.cmt to generate *)
  string ->
  (* module name *)
  binary_annots ->
  string option ->
  (* source file *)
  Env.t ->
  (* initial env *)
  Cmi_format.cmi_infos option ->
  (* if a .cmi was generated *)
  unit
(** [save_cmt filename modname binary_annots sourcefile initial_env cmi]
    writes a cmt(i) file.  *)

(* Miscellaneous functions *)

val read_magic_number : in_channel -> string

val clear : unit -> unit

val add_saved_type : binary_part -> unit
val get_saved_types : unit -> binary_part list
val set_saved_types : binary_part list -> unit

val record_value_dependency :
  Types.value_description -> Types.value_description -> unit

(*

  val is_magic_number : string -> bool
  val read : in_channel -> Env.cmi_infos option * t
  val write_magic_number : out_channel -> unit
  val write : out_channel -> t -> unit

  val find : string list -> string -> string
  val read_signature : 'a -> string -> Types.signature * 'b list * 'c list

*)
