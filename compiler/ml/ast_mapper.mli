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

(** {1 Apply mappers to compilation units} *)

val tool_name : unit -> string
(** Can be used within a ppx preprocessor to know which tool is
    calling it ["ocamlc"], ["ocamlopt"], ["ocamldoc"], ["ocamldep"],
    ["ocaml"], ...  Some global variables that reflect command-line
    options are automatically synchronized between the calling tool
    and the ppx preprocessor: {!Clflags.include_dirs},
    {!Config.load_path}, {!Clflags.open_modules}, {!Clflags.for_package},
    {!Clflags.debug}. *)

val apply : source:string -> target:string -> mapper -> unit
(** Apply a mapper (parametrized by the unit name) to a dumped
    parsetree found in the [source] file and put the result in the
    [target] file. The [structure] or [signature] field of the mapper
    is applied to the implementation or interface.  *)

val run_main : (string list -> mapper) -> unit
(** Entry point to call to implement a standalone -ppx rewriter from a
    mapper, parametrized by the command line arguments.  The current
    unit name can be obtained from {!Location.input_name}.  This
    function implements proper error reporting for uncaught
    exceptions. *)

(** {1 Registration API} *)

val register_function : (string -> (string list -> mapper) -> unit) ref

val register : string -> (string list -> mapper) -> unit
(** Apply the [register_function].  The default behavior is to run the
    mapper immediately, taking arguments from the process command
    line.  This is to support a scenario where a mapper is linked as a
    stand-alone executable.

    It is possible to overwrite the [register_function] to define
    "-ppx drivers", which combine several mappers in a single process.
    Typically, a driver starts by defining [register_function] to a
    custom implementation, then lets ppx rewriters (linked statically
    or dynamically) register themselves, and then run all or some of
    them.  It is also possible to have -ppx drivers apply rewriters to
    only specific parts of an AST.

    The first argument to [register] is a symbolic name to be used by
    the ppx driver.  *)

(** {1 Convenience functions to write mappers} *)

val map_opt : ('a -> 'b) -> 'a option -> 'b option

val extension_of_error : Location.error -> extension
(** Encode an error into an 'ocaml.error' extension node which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the error. *)

val attribute_of_warning : Location.t -> string -> attribute
(** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the warning. *)

(** {1 Helper functions to call external mappers} *)

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

(** {1 Cookies} *)

(** Cookies are used to pass information from a ppx processor to
    a further invocation of itself, when called from the OCaml
    toplevel (or other tools that support cookies). *)

val set_cookie : string -> Parsetree.expression -> unit
val get_cookie : string -> Parsetree.expression option
