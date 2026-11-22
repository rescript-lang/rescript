(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Alain Frisch, LexiFi                            *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree

(** {1 A generic Typedtree mapper} *)

type mapper = {
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  env: mapper -> Env.t -> Env.t;
  expr: mapper -> expression -> expression;
  extension_constructor:
    mapper -> extension_constructor -> extension_constructor;
  module_binding: mapper -> module_binding -> module_binding;
  module_coercion: mapper -> module_coercion -> module_coercion;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration:
    mapper -> module_type_declaration -> module_type_declaration;
  package_type: mapper -> package_type -> package_type;
  pat: mapper -> pattern -> pattern;
  row_field: mapper -> row_field -> row_field;
  object_field: mapper -> object_field -> object_field;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_declarations:
    mapper ->
    rec_flag * type_declaration list ->
    rec_flag * type_declaration list;
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_bindings:
    mapper -> rec_flag * value_binding list -> rec_flag * value_binding list;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

val default : mapper
