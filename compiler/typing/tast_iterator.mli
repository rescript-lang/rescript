(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                           Isaac "Izzy" Avram                           *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**
Allows the implementation of typed tree inspection using open recursion
*)

open Asttypes
open Typedtree

type iterator = {
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  env: iterator -> Env.t -> unit;
  expr: iterator -> expression -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_coercion: iterator -> module_coercion -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  package_type: iterator -> package_type -> unit;
  pat: iterator -> pattern -> unit;
  row_field: iterator -> row_field -> unit;
  object_field: iterator -> object_field -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_declarations: iterator -> rec_flag * type_declaration list -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_bindings: iterator -> rec_flag * value_binding list -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}

val default_iterator : iterator
