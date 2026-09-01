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

(* Compilation of pattern-matching *)

open Typedtree

val call_switcher_variant_constant :
  (Lambda.t option -> Lambda.t -> (int * (string * Lambda.t)) list -> Lambda.t)
  ref

val call_switcher_variant_constr :
  (Location.t ->
  Lambda.t option ->
  Lambda.t ->
  (int * (string * Lambda.t)) list ->
  Lambda.t)
  ref

val make_test_sequence_variant_constant :
  (Lambda.t option -> Lambda.t -> (int * (string * Lambda.t)) list -> Lambda.t)
  ref

(* Entry points to match compiler *)
val for_function :
  Location.t ->
  int ref option ->
  Lambda.t ->
  (pattern * Lambda.t) list ->
  partial ->
  Lambda.t
val for_trywith : Lambda.t -> (pattern * Lambda.t) list -> Lambda.t
val for_let : Location.t -> Lambda.t -> pattern -> Lambda.t -> Lambda.t
val for_multiple_match :
  Location.t ->
  Lambda.t list ->
  (pattern * Lambda.t) list ->
  partial ->
  Lambda.t

exception Cannot_flatten

val flatten_pattern : int -> pattern -> pattern list
