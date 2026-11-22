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

(* A case's right-hand side. The guard is kept apart from the body until the
   match compiler knows what it falls through to; it is not encoded as a term
   to be recognized by shape later. *)
type action

val unguarded : Lambda.t -> action

val guarded : guard:Lambda.t -> Lambda.t -> action

(* Entry points to match compiler *)
val for_function :
  Location.t ->
  int ref option ->
  Lambda.t ->
  (pattern * action) list ->
  partial ->
  Lambda.t
val for_trywith : Lambda.t -> (pattern * action) list -> Lambda.t
val for_let : Location.t -> Lambda.t -> pattern -> Lambda.t -> Lambda.t
val for_multiple_match :
  Location.t -> Lambda.t list -> (pattern * action) list -> partial -> Lambda.t

exception Cannot_flatten
