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

(** Type-checking of the module language and typed ast plugin hooks *)

open Types
open Format

val type_module : Env.t -> Parsetree.module_expr -> Typedtree.module_expr
val type_structure :
  Env.t ->
  Parsetree.structure ->
  Location.t ->
  Typedtree.structure * Types.signature * Env.t
val type_toplevel_phrase :
  Env.t -> Parsetree.structure -> Typedtree.structure * Types.signature * Env.t

val rescript_hide : Typedtree.structure_item_desc -> bool

val type_implementation_more :
  ?check_exists:unit ->
  string ->
  string ->
  string ->
  Env.t ->
  Parsetree.structure ->
  Typedtree.structure * Typedtree.module_coercion * Env.t * Types.signature

val transl_signature : Env.t -> Parsetree.signature -> Typedtree.signature
val check_nongen_schemes : Env.t -> Types.signature -> unit
val type_open_ :
  ?used_slot:bool ref ->
  ?toplevel:bool ->
  Asttypes.override_flag ->
  Env.t ->
  Location.t ->
  Longident.t Asttypes.loc ->
  Path.t * Env.t
val simplify_signature : signature -> signature

val path_of_module : Typedtree.module_expr -> Path.t option

val save_signature :
  string ->
  Typedtree.signature ->
  string ->
  string ->
  Env.t ->
  Cmi_format.cmi_infos ->
  unit

type error =
  | Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.error list
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of string * string * Warnings.loc
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error : Env.t -> formatter -> error -> unit
