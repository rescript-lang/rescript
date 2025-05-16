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
open Lambda

val call_switcher_variant_constant :
  (Location.t ->
  Lambda.lambda option ->
  Lambda.lambda ->
  (int * (string * Lambda.lambda)) list ->
  Ast_untagged_variants.switch_names option ->
  Lambda.lambda)
  ref

val call_switcher_variant_constr :
  (Location.t ->
  Lambda.lambda option ->
  Lambda.lambda ->
  (int * (string * Lambda.lambda)) list ->
  Ast_untagged_variants.switch_names option ->
  Lambda.lambda)
  ref

val make_test_sequence_variant_constant :
  (Lambda.lambda option ->
  Lambda.lambda ->
  (int * (string * Lambda.lambda)) list ->
  Lambda.lambda)
  ref

(* Entry points to match compiler *)
val for_function :
  Location.t ->
  int ref option ->
  lambda ->
  (pattern * lambda) list ->
  partial ->
  lambda
val for_trywith : lambda -> (pattern * lambda) list -> lambda
val for_let : Location.t -> lambda -> pattern -> lambda -> lambda
val for_multiple_match :
  Location.t -> lambda list -> (pattern * lambda) list -> partial -> lambda

val for_tupled_function :
  Location.t ->
  Ident.t list ->
  (pattern list * lambda) list ->
  partial ->
  lambda

exception Cannot_flatten

val flatten_pattern : int -> pattern -> pattern list

(* Expand stringswitch to  string test tree *)
val expand_stringswitch :
  Location.t -> lambda -> (string * lambda) list -> lambda option -> lambda

(* To be set by Lam_compile *)
val names_from_construct_pattern :
  (pattern -> Ast_untagged_variants.switch_names option) ref
