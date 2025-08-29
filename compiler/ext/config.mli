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

(* The directory containing the runtime module (@rescript/runtime) *)
val runtime_module_path : string

(* The directory containing the runtime artifacts (@rescript/runtime/lib/ocaml) *)
val standard_library : string

(* Directories in the search path for .cmi and .cmo files *)
val load_path : string list ref

val cmi_magic_number : string

(* Magic number for compiled interface files *)
val ast_intf_magic_number : string

(* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number : string

(* Magic number for file holding an implementation syntax tree *)
val cmt_magic_number : string
(* Magic number for compiled interface files *)
