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

(* Inclusion checks for the module language *)

open Typedtree
open Types
open Format

val modtypes :
  loc:Location.t -> Env.t -> module_type -> module_type -> module_coercion

val signatures : Env.t -> signature -> signature -> module_coercion

val compunit :
  Env.t -> string -> signature -> string -> signature -> module_coercion

val type_declarations :
  loc:Location.t ->
  Env.t ->
  Ident.t ->
  type_declaration ->
  type_declaration ->
  unit

val print_coercion : formatter -> module_coercion -> unit

type symptom =
  | Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of
      Ident.t
      * type_declaration
      * type_declaration
      * Includecore.type_mismatch list
  | Extension_constructors of
      Ident.t * extension_constructor * extension_constructor
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Interface_mismatch of string * string
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of Ident.t
  | Body of Ident.t
type error = pos list * Env.t * symptom

exception Error of error list

val report_error : formatter -> error list -> unit
val expand_module_alias : Env.t -> pos list -> Path.t -> Types.module_type
