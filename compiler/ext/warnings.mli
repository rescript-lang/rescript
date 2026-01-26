(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type top_level_unit_help = FunctionCall | Other

type t =
  | Comment_start (*  1 *)
  | Comment_not_end (*  2 *)
  | Deprecated of string * loc * loc * bool (*  3 *)
  | Fragile_match of string (*  4 *)
  | Partial_application (*  5 *)
  | Method_override of string list (*  7 *)
  | Partial_match of string (*  8 *)
  | Non_closed_record_pattern of string (*  9 *)
  | Statement_type (* 10 *)
  | Unused_match (* 11 *)
  | Unused_pat (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash (* 14 *)
  | Implicit_public_methods of string list (* 15 *)
  | Unerasable_optional_argument (* 16 *)
  | Unused_argument (* 20 *)
  | Nonreturning_statement (* 21 *)
  | Preprocessor of string (* 22 *)
  | Useless_record_with (* 23 *)
  | Bad_module_name of string (* 24 *)
  | All_clauses_guarded (* 8, used to be 25 *)
  | Unused_var of string (* 26 *)
  | Unused_var_strict of string (* 27 *)
  | Wildcard_arg_to_constant_constr (* 28 *)
  | Eol_in_string (* 29 *)
  | Duplicate_definitions of string * string * string * string (* 30 *)
  | Unused_value_declaration of string (* 32 *)
  | Unused_open of string (* 33 *)
  | Unused_type_declaration of string (* 34 *)
  | Unused_for_index of string (* 35 *)
  | Unused_constructor of string * bool * bool (* 37 *)
  | Unused_extension of string * bool * bool * bool (* 38 *)
  | Unused_rec_flag (* 39 *)
  | Ambiguous_name of string list * string list * bool (* 41 *)
  | Nonoptional_label of string (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Attribute_payload of string * string (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string * string option (* 49 *)
  | Bad_docstring of bool (* 50 *)
  | Fragile_literal_pattern (* 52 *)
  | Misplaced_attribute of string (* 53 *)
  | Duplicated_attribute of string (* 54 *)
  | Unreachable_case (* 56 *)
  | Ambiguous_pattern of string list (* 57 *)
  | Unused_module of string (* 60 *)
  | Constraint_on_gadt (* 62 *)
  | Bs_unused_attribute of string (* 101 *)
  | Bs_polymorphic_comparison (* 102 *)
  | Bs_ffi_warning of string (* 103 *)
  | Bs_derive_warning of string (* 104 *)
  | Bs_fragile_external of string (* 105 *)
  | Bs_unimplemented_primitive of string (* 106 *)
  | Bs_integer_literal_overflow (* 107 *)
  | Bs_uninterpreted_delimiters of string (* 108 *)
  | Bs_toplevel_expression_unit of
      (string * top_level_unit_help) option (* 109 *)
  | Bs_todo of string option (* 110 *)

val parse_options : bool -> string -> unit

val without_warnings : (unit -> 'a) -> 'a

val is_active : t -> bool

val is_error : t -> bool

type reporting_information = {
  number: int;
  message: string;
  is_error: bool;
  sub_locs: (loc * string) list;
}

val report : t -> [`Active of reporting_information | `Inactive]

exception Errors

val check_fatal : unit -> unit

val reset_fatal : unit -> unit

val help_warnings : unit -> unit

type state

val backup : unit -> state

val restore : state -> unit

val mk_lazy : (unit -> 'a) -> 'a Lazy.t
(** Like [Lazy.of_fun], but the function is applied with
        the warning settings at the time [mk_lazy] is called. *)

val has_warnings : bool ref

val nerrors : int ref

val message : t -> string

val number : t -> int

val reset : unit -> unit

val loc_to_string : loc -> string
(**
Turn the location into a string with (line,column--line,column) format.
*)
