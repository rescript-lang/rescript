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

(* Basic operations on core types *)

open Asttypes
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet : Set.S with type elt = type_expr
module TypeMap : Map.S with type key = type_expr
module TypeHash : Hashtbl.S with type key = type_expr

(**** Levels ****)

val generic_level : int

val newty2 : int -> type_desc -> type_expr
(* Create a type *)

val newgenty : type_desc -> type_expr
(* Create a generic type *)

val newgenvar : ?name:string -> unit -> type_expr
(* Return a fresh generic variable *)

(* Use Tsubst instead
   val newmarkedvar: int -> type_expr
           (* Return a fresh marked variable *)
   val newmarkedgenvar: unit -> type_expr
           (* Return a fresh marked generic variable *)
*)

(**** Types ****)

val is_Tvar : type_expr -> bool
val is_Tunivar : type_expr -> bool
val is_Tconstr : type_expr -> bool
val dummy_method : label
val default_mty : module_type option -> module_type

val repr : type_expr -> type_expr
(* Return the canonical representative of a type. *)

val field_kind_repr : field_kind -> field_kind
(* Return the canonical representative of an object field
   kind. *)

val commu_repr : commutable -> commutable
(* Return the canonical representative of a commutation lock *)

(**** polymorphic variants ****)

val row_repr : row_desc -> row_desc
(* Return the canonical representative of a row description *)

val row_field_repr : row_field -> row_field
val row_field : label -> row_desc -> row_field
(* Return the canonical representative of a row field *)

val row_more : row_desc -> type_expr
(* Return the extension variable of the row *)

val row_fixed : row_desc -> bool
(* Return whether the row should be treated as fixed or not *)

val static_row : row_desc -> bool
(* Return whether the row is static or not *)

val hash_variant : label -> int
(* Hash function for variant tags *)

val proxy : type_expr -> type_expr
(* Return the proxy representative of the type: either itself
   or a row variable *)

(**** Utilities for private abbreviations with fixed rows ****)
val row_of_type : type_expr -> type_expr
val has_constr_row : type_expr -> bool
val is_row_name : string -> bool
val is_constr_row : allow_ident:bool -> type_expr -> bool

(**** Utilities for type traversal ****)

val iter_type_expr : (type_expr -> unit) -> type_expr -> unit
(* Iteration on types *)

val iter_row : (type_expr -> unit) -> row_desc -> unit
(* Iteration on types in a row *)

val iter_abbrev : (type_expr -> unit) -> abbrev_memo -> unit
(* Iteration on types in an abbreviation list *)

type type_iterators = {
  it_signature: type_iterators -> signature -> unit;
  it_signature_item: type_iterators -> signature_item -> unit;
  it_value_description: type_iterators -> value_description -> unit;
  it_type_declaration: type_iterators -> type_declaration -> unit;
  it_extension_constructor: type_iterators -> extension_constructor -> unit;
  it_module_declaration: type_iterators -> module_declaration -> unit;
  it_modtype_declaration: type_iterators -> modtype_declaration -> unit;
  it_module_type: type_iterators -> module_type -> unit;
  it_type_kind: type_iterators -> type_kind -> unit;
  it_do_type_expr: type_iterators -> type_expr -> unit;
  it_type_expr: type_iterators -> type_expr -> unit;
  it_path: Path.t -> unit;
}
val type_iterators : type_iterators
(* Iteration on arbitrary type information.
   [it_type_expr] calls [mark_type_node] to avoid loops. *)

val unmark_iterators : type_iterators
(* Unmark any structure containing types. See [unmark_type] below. *)

val copy_type_desc :
  ?keep_names:bool -> (type_expr -> type_expr) -> type_desc -> type_desc
(* Copy on types *)

val copy_row :
  (type_expr -> type_expr) -> bool -> row_desc -> bool -> type_expr -> row_desc
val copy_kind : field_kind -> field_kind

val save_desc : type_expr -> type_desc -> unit
(* Save a type description *)

val dup_kind : field_kind option ref -> unit
(* Save a None field_kind, and make it point to a fresh Fvar *)

val cleanup_types : unit -> unit
(* Restore type descriptions *)

val lowest_level : int
(* Marked type: ty.level < lowest_level *)

val pivot_level : int
(* Type marking: ty.level <- pivot_level - ty.level *)

val mark_type : type_expr -> unit
(* Mark a type *)

val mark_type_node : type_expr -> unit
(* Mark a type node (but not its sons) *)

val mark_type_params : type_expr -> unit
(* Mark the sons of a type node *)

val unmark_type : type_expr -> unit
val unmark_type_decl : type_declaration -> unit
val unmark_extension_constructor : extension_constructor -> unit

(**** Memorization of abbreviation expansion ****)

val find_expans : private_flag -> Path.t -> abbrev_memo -> type_expr option
(* Look up a memorized abbreviation *)

val cleanup_abbrev : unit -> unit
(* Flush the cache of abbreviation expansions.
   When some types are saved (using [output_value]), this
   function MUST be called just before. *)

val memorize_abbrev :
  abbrev_memo ref -> private_flag -> Path.t -> type_expr -> type_expr -> unit
(* Add an expansion in the cache *)

val forget_abbrev : abbrev_memo ref -> Path.t -> unit
(* Remove an abbreviation from the cache *)

(**** Utilities for labels ****)

val is_optional : Noloc.arg_label -> bool
val is_optional_loc : arg_label -> bool
val label_name : Noloc.arg_label -> label
val label_loc_name : arg_label -> label

(* Returns the label name with first character '?' or '~' as appropriate. *)
val prefixed_label_name : Noloc.arg_label -> label

type sargs = (arg_label * Parsetree.expression) list

val extract_label :
  label -> sargs -> (arg_label * Parsetree.expression * sargs) option
(* actual label, value, new list with the same order *)

val label_assoc : arg_label -> sargs -> bool
(**** Utilities for backtracking ****)

type snapshot
(* A snapshot for backtracking *)

val snapshot : unit -> snapshot
(* Make a snapshot for later backtracking. Costs nothing *)

val backtrack : snapshot -> unit
(* Backtrack to a given snapshot. Only possible if you have
   not already backtracked to a previous snapshot.
   Calls [cleanup_abbrev] internally *)

val undo_compress : snapshot -> unit
(* Backtrack only path compression. Only meaningful if you have
   not already backtracked to a previous snapshot.
   Does not call [cleanup_abbrev] *)

(* Functions to use when modifying a type (only Ctype?) *)
val link_type : type_expr -> type_expr -> unit
(* Set the desc field of [t1] to [Tlink t2], logging the old
   value if there is an active snapshot *)

val set_level : type_expr -> int -> unit
val set_name :
  (Path.t * type_expr list) option ref ->
  (Path.t * type_expr list) option ->
  unit
val set_row_field : row_field option ref -> row_field -> unit
val set_univar : type_expr option ref -> type_expr -> unit
val set_kind : field_kind option ref -> field_kind -> unit
val set_commu : commutable ref -> commutable -> unit
val set_typeset : TypeSet.t ref -> TypeSet.t -> unit
(* Set references, logging the old value *)

val log_type : type_expr -> unit
(* Log the old value of a type, before modifying it by hand *)

(**** Forward declarations ****)
val print_raw : (Format.formatter -> type_expr -> unit) ref

val iter_type_expr_kind : (type_expr -> unit) -> type_kind -> unit

val iter_type_expr_cstr_args :
  (type_expr -> unit) -> constructor_arguments -> unit
val map_type_expr_cstr_args :
  (type_expr -> type_expr) -> constructor_arguments -> constructor_arguments
