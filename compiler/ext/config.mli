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

(* Directories in the search path for .cmi and .cmo files *)
val load_path : string list ref

val cmi_magic_number : string
(* Magic number for compiled interface files *)

val ast_intf_magic_number : string
(* Magic number for a marshaled current-parsetree signature (layout changes
   across compiler versions; distinct from the frozen Parsetree0 wire format) *)

val ast_impl_magic_number : string
(* Magic number for a marshaled current-parsetree structure (layout changes
   across compiler versions; distinct from the frozen Parsetree0 wire format) *)

val ast0_intf_magic_number : string
(* Magic number for a frozen Parsetree0 (OCaml 4.06) interface syntax tree, as
   used on the external-PPX wire *)

val ast0_impl_magic_number : string
(* Magic number for a frozen Parsetree0 (OCaml 4.06) implementation syntax
   tree, as used on the external-PPX wire *)

val cmt_magic_number : string
(* Magic number for compiled interface files *)
