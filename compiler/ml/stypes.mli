(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2003 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Recording and dumping (partial) type information *)

(* Clflags.save_types must be true *)

open Typedtree

type annotation =
  | Ti_pat of pattern
  | Ti_expr of expression
  | Ti_class of unit
  | Ti_mod of module_expr
  | An_call of Location.t * Annot.call
  | An_ident of Location.t * string * Annot.ident

val record : annotation -> unit
val record_phrase : Location.t -> unit
val dump : string option -> unit

val get_location : annotation -> Location.t
val get_info : unit -> annotation list
