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

open Lambda

open Format

val structured_constant : formatter -> structured_constant -> unit
val lambda : formatter -> Lambda.t -> unit

val primitive : formatter -> Lambda.primitive -> unit

val serialize : string -> Lambda.t -> unit
(** Print a term to a file, unwrapped: used for the -debug-ir dumps. *)

val lambda_to_string : Lambda.t -> string
