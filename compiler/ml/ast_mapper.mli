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

(** Parsetree mappers

  {!mapper} implements AST rewriting using open recursion. A typical
  mapper is based on {!default_mapper}, a deep identity mapper, and
  falls back on it for the syntax it does not modify. For example:

  {[
open Parsetree
open Ast_mapper

let test_mapper =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
        Ast_helper.Exp.constant (Ast_helper.Const.int 42)
      | other -> default_mapper.expr mapper other; }

let rewrite (str : structure) = test_mapper.structure test_mapper str]}

  This mapper replaces [[%test]] in expressions with the constant [42].
  The compiler's built-in rewriters ({!Bs_builtin_ppx}, {!Jsx_ppx}) are
  mappers of this kind and run inside [bsc].

  External rewriters passed to [bsc] with [-ppx] are separate
  executables. {!Cmd_ppx_apply} prepends the [ocaml.ppx.context]
  attribute ({!add_ppx_context_str}, {!add_ppx_context_sig}), writes the
  AST to a temporary file as the magic number of {!Ml_binary}, the
  source file name and the marshalled {!Parsetree0} structure or
  signature, and runs [ppx input output]. The executable writes its
  result to [output] in the same format; {!Cmd_ppx_apply} reads it back,
  converts it to {!Parsetree}, and removes the context attribute
  ({!drop_ppx_context_str}, {!drop_ppx_context_sig}).
  *)

open Parsetree

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
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}
(** A mapper record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the mapper to be applied to children in the syntax
    tree. *)

val default_mapper : mapper
(** A default mapper, which implements a "deep identity" mapping. *)

(** {1 Convenience functions to write mappers} *)

val extension_of_error : Location.error -> extension
(** Encode an error into an 'ocaml.error' extension node which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the error. *)

val add_ppx_context_str :
  tool_name:string -> Parsetree.structure -> Parsetree.structure
(** Extract information from the current environment and encode it
    into an attribute which is prepended to the list of structure
    items in order to pass the information to an external
    processor. *)

val add_ppx_context_sig :
  tool_name:string -> Parsetree.signature -> Parsetree.signature
(** Same as [add_ppx_context_str], but for signatures. *)

val drop_ppx_context_str :
  restore:bool -> Parsetree.structure -> Parsetree.structure
(** Drop the ocaml.ppx.context attribute from a structure.  If
    [restore] is true, also restore the associated data in the current
    process. *)

val drop_ppx_context_sig :
  restore:bool -> Parsetree.signature -> Parsetree.signature
(** Same as [drop_ppx_context_str], but for signatures. *)
