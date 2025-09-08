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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

val transl_exp : Typedtree.expression -> Lambda.lambda

val transl_let :
  Asttypes.rec_flag ->
  Typedtree.value_binding list ->
  Lambda.lambda ->
  Lambda.lambda

val transl_primitive :
  Location.t ->
  Primitive.description ->
  Env.t ->
  Types.type_expr ->
  Lambda.lambda

val transl_extension_constructor :
  Env.t -> Path.t option -> Typedtree.extension_constructor -> Lambda.lambda

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
  (Typedtree.module_coercion ->
  Path.t option ->
  Typedtree.module_expr ->
  Lambda.lambda)
  ref
