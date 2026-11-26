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

val header : string
(** The first line of every generated JavaScript file. *)

(* Directories in the search path for .cmi and .cmo files *)
val get_load_path : unit -> string list
val set_load_path : string list -> unit

val cmi_magic_number : string
(* Magic number for compiled interface files *)

val ast_intf_magic_number : string
(* Magic number for a marshaled current-parsetree signature (layout changes
   across compiler versions; distinct from the frozen Parsetree0 wire format) *)

val ast_impl_magic_number : string
(* Magic number for a marshaled current-parsetree structure (layout changes
   across compiler versions; distinct from the frozen Parsetree0 wire format) *)

val cmt_magic_number : string
(* Magic number for typed-tree files (.cmt, .cmti) *)
