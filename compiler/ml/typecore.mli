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

(* Type inference for the core language *)

open Asttypes
open Types
open Format

val is_nonexpansive : Typedtree.expression -> bool

val type_binding :
  Env.t ->
  rec_flag ->
  Parsetree.value_binding list ->
  Annot.ident option ->
  Typedtree.value_binding list * Env.t
val type_expression : Env.t -> Parsetree.expression -> Typedtree.expression
val check_partial :
  ?lev:int ->
  Env.t ->
  type_expr ->
  Location.t ->
  Typedtree.case list ->
  Typedtree.partial
val type_exp : Env.t -> Parsetree.expression -> Typedtree.expression
val type_approx : Env.t -> Parsetree.expression -> type_expr

val option_some : Typedtree.expression -> Typedtree.expression
val option_none : type_expr -> Location.t -> Typedtree.expression
val extract_option_type : Env.t -> type_expr -> type_expr
val iter_pattern : (Typedtree.pattern -> unit) -> Typedtree.pattern -> unit
val generalizable : int -> type_expr -> bool

val id_of_pattern : Typedtree.pattern -> Ident.t option
val name_pattern : string -> Typedtree.case list -> Ident.t

type error =
  | Polymorphic_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Or_pattern_type_clash of Ident.t * (type_expr * type_expr) list
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of
      (type_expr * type_expr) list
      * Error_message_utils.type_clash_context option
  | Apply_non_function of type_expr
  | Apply_wrong_label of Noloc.arg_label * type_expr
  | Label_multiply_defined of {
      label: string;
      jsx_component_info: Error_message_utils.jsx_prop_error_info option;
    }
  | Labels_missing of {
      labels: string list;
      jsx_component_info: Error_message_utils.jsx_prop_error_info option;
    }
  | Label_not_mutable of Longident.t
  | Wrong_name of string * type_expr * string * Path.t * string * string list
  | Name_type_mismatch of
      string * Longident.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Undefined_method of type_expr * string * string list option
  | Private_type of type_expr
  | Private_label of Longident.t * type_expr
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Too_many_arguments of bool * type_expr
  | Abstract_wrong_label of Noloc.arg_label * type_expr
  | Scoping_let_module of string * type_expr
  | Not_a_variant_type of Longident.t
  | Incoherent_label_order
  | Less_general of string * (type_expr * type_expr) list
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Recursive_local_constraint of (type_expr * type_expr) list
  | Unexpected_existential
  | Unqualified_gadt_pattern of Path.t * string
  | Invalid_interval
  | Invalid_for_loop_index
  | No_value_clauses
  | Exception_pattern_below_toplevel
  | Inlined_record_escape
  | Inlined_record_expected
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Illegal_letrec_pat
  | Empty_record_literal
  | Uncurried_arity_mismatch of type_expr * int * int
  | Field_not_optional of string * type_expr
  | Type_params_not_supported of Longident.t
  | Field_access_on_dict_type
exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error : Env.t -> formatter -> error -> unit
(* Deprecated.  Use Location.{error_of_exn, report_error}. *)

(* Forward declaration, to be filled in by Typemod.type_module *)
val type_module : (Env.t -> Parsetree.module_expr -> Typedtree.module_expr) ref

(* Forward declaration, to be filled in by Typemod.type_open *)
val type_open :
  (?used_slot:bool ref ->
  override_flag ->
  Env.t ->
  Location.t ->
  Longident.t loc ->
  Path.t * Env.t)
  ref

(* Forward declaration, to be filled in by Typemod.type_package *)
val type_package :
  (Env.t ->
  Parsetree.module_expr ->
  Path.t ->
  Longident.t list ->
  Typedtree.module_expr * type_expr list)
  ref

val constant : Parsetree.constant -> (Asttypes.constant, error) result
