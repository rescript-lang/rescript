(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* System configuration *)

val version : string
(* The current version number of the system *)

val standard_library : string
(* The directory containing the standard libraries *)

val load_path : string list ref
(* Directories in the search path for .cmi and .cmo files *)

val cmi_magic_number : string

(* Magic number for compiled interface files *)
val ast_intf_magic_number : string

(* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number : string

(* Magic number for file holding an implementation syntax tree *)
val cmt_magic_number : string
(* Magic number for compiled interface files *)

val print_config : out_channel -> unit
