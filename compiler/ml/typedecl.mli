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

(* Typing of type definitions and primitive definitions *)

open Types
open Format

val transl_type_decl :
  Env.t ->
  Asttypes.rec_flag ->
  Parsetree.type_declaration list ->
  Typedtree.type_declaration list * Env.t

val transl_exception :
  Env.t ->
  Parsetree.extension_constructor ->
  Typedtree.extension_constructor * Env.t

val transl_type_extension :
  bool ->
  Env.t ->
  Location.t ->
  Parsetree.type_extension ->
  Typedtree.type_extension * Env.t

val transl_value_decl :
  Env.t ->
  Location.t ->
  Parsetree.value_description ->
  Typedtree.value_description * Env.t

val transl_with_constraint :
  Env.t ->
  Ident.t ->
  Path.t option ->
  Types.type_declaration ->
  Parsetree.type_declaration ->
  Typedtree.type_declaration

val abstract_type_decl : int -> type_declaration
val approx_type_decl :
  Parsetree.type_declaration list -> (Ident.t * type_declaration) list
val check_recmod_typedecl :
  Env.t -> Location.t -> Ident.t list -> Path.t -> type_declaration -> unit
val check_coherence : Env.t -> Location.t -> Ident.t -> type_declaration -> unit

(* for fixed types *)
val is_fixed_type : Parsetree.type_declaration -> bool

(* for typeopt.ml *)
val get_unboxed_type_representation : Env.t -> type_expr -> type_expr option
val is_not_undefined_attr : Parsetree.attribute -> bool

type native_repr_kind = Unboxed | Untagged

type error

exception Error of Location.t * error

val report_error : formatter -> error -> unit
