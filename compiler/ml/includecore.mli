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

(* Inclusion checks for the core language *)

open Typedtree
open Types

exception Dont_match

type type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Field_type of Ident.t
  | Field_mutable of Ident.t
  | Field_optional of Ident.t
  | Field_arity of Ident.t
  | Field_names of int * string * string
  | Field_missing of bool * Ident.t
  | Record_representation of record_representation * record_representation
  | Unboxed_representation of bool
  | Immediate
  | Tag_name
  | Variant_representation of Ident.t

val value_descriptions :
  loc:Location.t ->
  Env.t ->
  Ident.t ->
  value_description ->
  value_description ->
  module_coercion

val type_declarations :
  ?equality:bool ->
  loc:Location.t ->
  Env.t ->
  string ->
  type_declaration ->
  Ident.t ->
  type_declaration ->
  type_mismatch list

val extension_constructors :
  loc:Location.t ->
  Env.t ->
  Ident.t ->
  extension_constructor ->
  extension_constructor ->
  bool
(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)

val report_type_mismatch :
  string -> string -> string -> Format.formatter -> type_mismatch list -> unit
