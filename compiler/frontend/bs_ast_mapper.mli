(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The interface of a -ppx rewriter

    A -ppx rewriter is a program that accepts a serialized abstract syntax
    tree and outputs another, possibly modified, abstract syntax tree.
    This module encapsulates the interface between the compiler and
    the -ppx rewriters, handling such details as the serialization format,
    forwarding of command-line flags, and storing state.

    {!mapper} allows to implement AST rewriting using open recursion.
    A typical mapper would be based on {!default_mapper}, a deep
    identity mapper, and will fall back on it for handling the syntax it
    does not modify. For example:

    {[
      open Asttypes
      open Parsetree
      open Ast_mapper

      let test_mapper argv =
        { default_mapper with
          expr = fun mapper expr ->
            match expr with
            | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
              Ast_helper.Exp.constant (Const_int 42)
            | other -> default_mapper.expr mapper other; }

      let () =
        register "ppx_test" test_mapper]}

    This -ppx rewriter, which replaces [[%test]] in expressions with
    the constant [42], can be compiled using
    [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

*)

open! Parsetree

(** {1 A generic Parsetree mapper} *)

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  constructor_declaration:
    mapper -> constructor_declaration -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor:
    mapper -> extension_constructor -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration:
    mapper -> module_type_declaration -> module_type_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_declaration_list:
    mapper -> type_declaration list -> type_declaration list;
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_bindings_rec: mapper -> value_binding list -> value_binding list;
  value_bindings: mapper -> value_binding list -> value_binding list;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}
(** A mapper record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the mapper to be applied to children in the syntax
    tree. *)

val default_mapper : mapper
(** A default mapper, which implements a "deep identity" mapping. *)
