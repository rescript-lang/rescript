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

(* Operations on core types *)

open Asttypes
open Types

type subtype_context =
  | Generic of {errorCode: string}
      (** A generic subtype error, intended to be extended to be handled later. *)
  | Coercion_target_variant_not_unboxed of {
      variant_name: Path.t;
      primitive: Path.t;
    }  (** Coercing a primitive to a variant that is not unboxed. *)
  | Coercion_target_variant_does_not_cover_type of {
      variant_name: Path.t;
      primitive: Path.t;
    }
      (** Coercing a primitive to a variant that does not have a catch-all case. *)
  | Variant_constructor_runtime_representation_mismatch of {
      variant_name: Path.t;
      issues: Variant_coercion.variant_runtime_representation_issue list;
    }
      (** A variant constructor's runtime representation does not match the target variant. *)
  | Variant_configurations_mismatch of {
      left_variant_name: Path.t;
      right_variant_name: Path.t;
      issue: Variant_coercion.variant_configuration_issue;
    }  (** Variants are configured differently. *)
  | Different_type_kinds of {
      left_typename: Path.t;
      right_typename: Path.t;
      left_type_kind: type_kind;
      right_type_kind: type_kind;
    }  (** The types are of different kinds. *)
  | Record_fields_mismatch of {
      left_record_name: Path.t;
      right_record_name: Path.t;
      issues: Record_coercion.record_field_subtype_violation list;
    }  (** Records have fields that are not compatible. *)

type type_pairs = (type_expr * type_expr) list
exception Unify of type_pairs
exception Tags of label * label
exception Subtype of type_pairs * type_pairs * subtype_context option
exception Cannot_expand
exception Cannot_apply
exception Recursive_abbrev
exception Unification_recursive_abbrev of type_pairs

val init_def : int -> unit
(* Set the initial variable level *)

val begin_def : unit -> unit
(* Raise the variable level by one at the beginning of a definition. *)

val end_def : unit -> unit
(* Lower the variable level by one at the end of a definition *)

val begin_class_def : unit -> unit
val raise_nongen_level : unit -> unit
val reset_global_level : unit -> unit
(* Reset the global level before typing an expression *)

val increase_global_level : unit -> int
val restore_global_level : int -> unit
(* This pair of functions is only used in Typetexp *)

type levels = {
  current_level: int;
  nongen_level: int;
  global_level: int;
  saved_level: (int * int) list;
}
val save_levels : unit -> levels
val set_levels : levels -> unit

val newty : type_desc -> type_expr
val newvar : ?name:string -> unit -> type_expr
val newvar2 : ?name:string -> int -> type_expr
(* Return a fresh variable *)

val new_global_var : ?name:string -> unit -> type_expr
(* Return a fresh variable, bound at toplevel
   (as type variables ['a] in type constraints). *)

val newobj : type_expr -> type_expr
val newconstr : Path.t -> type_expr list -> type_expr
val none : type_expr
(* A dummy type expression *)

val repr : type_expr -> type_expr
(* Return the canonical representative of a type. *)

val object_fields : type_expr -> type_expr
val flatten_fields :
  type_expr -> (string * field_kind * type_expr) list * type_expr

(* Transform a field type into a list of pairs label-type *)
(* The fields are sorted *)
val associate_fields :
  (string * field_kind * type_expr) list ->
  (string * field_kind * type_expr) list ->
  (string * field_kind * type_expr * field_kind * type_expr) list
  * (string * field_kind * type_expr) list
  * (string * field_kind * type_expr) list
val opened_object : type_expr -> bool
val close_object : type_expr -> unit
val row_variable : type_expr -> type_expr
(* Return the row variable of an open object type *)

val set_object_name :
  Ident.t -> type_expr -> type_expr list -> type_expr -> unit
val remove_object_name : type_expr -> unit
val hide_private_methods : type_expr -> unit
val find_cltype_for_path : Env.t -> Path.t -> type_declaration * type_expr
val lid_of_path : ?hash:string -> Path.t -> Longident.t

val sort_row_fields : (label * row_field) list -> (label * row_field) list
val merge_row_fields :
  (label * row_field) list ->
  (label * row_field) list ->
  (label * row_field) list
  * (label * row_field) list
  * (label * row_field * row_field) list
val filter_row_fields :
  bool -> (label * row_field) list -> (label * row_field) list

val generalize : type_expr -> unit
(* Generalize in-place the given type *)

val generalize_expansive : Env.t -> type_expr -> unit
(* Generalize the covariant part of a type, making
   contravariant branches non-generalizable *)

val generalize_global : type_expr -> unit
(* Generalize the structure of a type, lowering variables
   to !global_level *)

val generalize_structure : type_expr -> unit
(* Same, but variables are only lowered to !current_level *)

val correct_levels : type_expr -> type_expr
(* Returns a copy with decreasing levels *)

val limited_generalize : type_expr -> type_expr -> unit
(* Only generalize some part of the type
   Make the remaining of the type non-generalizable *)

val instance : ?partial:bool -> Env.t -> type_expr -> type_expr

(* Take an instance of a type scheme *)
(* partial=None  -> normal
   partial=false -> newvar() for non generic subterms
   partial=true  -> newty2 ty.level Tvar for non generic subterms *)
val instance_def : type_expr -> type_expr
(* use defaults *)

val generic_instance : Env.t -> type_expr -> type_expr
(* Same as instance, but new nodes at generic_level *)

val instance_list : Env.t -> type_expr list -> type_expr list
(* Take an instance of a list of type schemes *)

val instance_constructor :
  ?in_pattern:Env.t ref * int ->
  constructor_description ->
  type_expr list * type_expr
(* Same, for a constructor *)

val instance_parameterized_type :
  ?keep_names:bool -> type_expr list -> type_expr -> type_expr list * type_expr
val instance_parameterized_type_2 :
  type_expr list ->
  type_expr list ->
  type_expr ->
  type_expr list * type_expr list * type_expr
val instance_declaration : type_declaration -> type_declaration
val instance_poly :
  ?keep_names:bool ->
  bool ->
  type_expr list ->
  type_expr ->
  type_expr list * type_expr
(* Take an instance of a type scheme containing free univars *)

val instance_label :
  bool -> label_description -> type_expr list * type_expr * type_expr
(* Same, for a label *)

val apply : Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
(* [apply [p1...pN] t [a1...aN]] match the arguments [ai] to
   the parameters [pi] and returns the corresponding instance of
   [t]. Exception [Cannot_apply] is raised in case of failure. *)

val expand_head_once : Env.t -> type_expr -> type_expr
val expand_head : Env.t -> type_expr -> type_expr
val try_expand_once_opt : Env.t -> type_expr -> type_expr

val expand_head_opt : Env.t -> type_expr -> type_expr
(** The compiler's own version of [expand_head] necessary for type-based
    optimisations. *)

val full_expand : Env.t -> type_expr -> type_expr
val extract_concrete_typedecl :
  Env.t -> type_expr -> Path.t * Path.t * type_declaration
(* Return the original path of the types, and the first concrete
   type declaration found expanding it.
   Raise [Not_found] if none appears or not a type constructor. *)

val enforce_constraints : Env.t -> type_expr -> unit

val unify : Env.t -> type_expr -> type_expr -> unit
(* Unify the two types given. Raise [Unify] if not possible. *)

val unify_gadt :
  newtype_level:int -> Env.t ref -> type_expr -> type_expr -> unit
(* Unify the two types given and update the environment with the
   local constraints. Raise [Unify] if not possible. *)

val unify_var : Env.t -> type_expr -> type_expr -> unit
(* Same as [unify], but allow free univars when first type
   is a variable. *)

val with_passive_variants : ('a -> 'b) -> 'a -> 'b
(* Call [f] in passive_variants mode, for exhaustiveness check. *)

val filter_arrow :
  env:Env.t ->
  arity:arity ->
  type_expr ->
  Noloc.arg_label ->
  type_expr * type_expr
(* A special case of unification (with l:'a -> 'b). *)

val filter_method : Env.t -> string -> private_flag -> type_expr -> type_expr
(* A special case of unification (with {m : 'a; 'b}). *)

val check_filter_method : Env.t -> string -> private_flag -> type_expr -> unit
(* A special case of unification (with {m : 'a; 'b}), returning unit. *)

val occur_in : Env.t -> type_expr -> type_expr -> bool
val deep_occur : type_expr -> type_expr -> bool
val filter_self_method :
  Env.t ->
  string ->
  private_flag ->
  (Ident.t * type_expr) Meths.t ref ->
  type_expr ->
  Ident.t * type_expr
val moregeneral : Env.t -> bool -> type_expr -> type_expr -> bool
(* Check if the first type scheme is more general than the second. *)

val rigidify : type_expr -> type_expr list
(* "Rigidify" a type and return its type variable *)

val all_distinct_vars : Env.t -> type_expr list -> bool
(* Check those types are all distinct type variables *)

val matches : Env.t -> type_expr -> type_expr -> bool
(* Same as [moregeneral false], implemented using the two above
   functions and backtracking. Ignore levels *)

type class_match_failure =
  | CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of Env.t * (type_expr * type_expr) list
  | CM_Parameter_mismatch of Env.t * (type_expr * type_expr) list
  | CM_Val_type_mismatch of string * Env.t * (type_expr * type_expr) list
  | CM_Meth_type_mismatch of string * Env.t * (type_expr * type_expr) list
  | CM_Non_mutable_value of string
  | CM_Non_concrete_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string * string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string
val equal : Env.t -> bool -> type_expr list -> type_expr list -> bool
(* [equal env [x1...xn] tau [y1...yn] sigma]
   checks whether the parameterized types
   [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)

val enlarge_type : Env.t -> type_expr -> type_expr * bool
(* Make a type larger, flag is true if some pruning had to be done *)

val subtype : Env.t -> type_expr -> type_expr -> unit -> unit
(* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
   It accumulates the constraints the type variables must
   enforce and returns a function that enforces this
   constraints. *)

val nondep_type : Env.t -> Ident.t -> type_expr -> type_expr
(* Return a type equivalent to the given type but without
   references to the given module identifier. Raise [Not_found]
   if no such type exists. *)

val nondep_type_decl :
  Env.t -> Ident.t -> Ident.t -> bool -> type_declaration -> type_declaration
(* Same for type declarations. *)

val nondep_extension_constructor :
  Env.t -> Ident.t -> extension_constructor -> extension_constructor

(* Same for extension constructor *)
(*val correct_abbrev: Env.t -> Path.t -> type_expr list -> type_expr -> unit*)
val cyclic_abbrev : Env.t -> Ident.t -> type_expr -> bool
val is_contractive : Env.t -> Path.t -> bool
val normalize_type : Env.t -> type_expr -> unit

val closed_schema : Env.t -> type_expr -> bool
(* Check whether the given type scheme contains no non-generic
   type variables *)

val free_variables : ?env:Env.t -> type_expr -> type_expr list
(* If env present, then check for incomplete definitions too *)

val closed_type_decl : type_declaration -> type_expr option
val closed_extension_constructor : extension_constructor -> type_expr option
type closed_class_failure =
  | CC_Method of type_expr * bool * string * type_expr
  | CC_Value of type_expr * bool * string * type_expr

val unalias : type_expr -> type_expr
val arity : type_expr -> int
(* Return the arity (as for curried functions) of the given type. *)

val collapse_conj_params : Env.t -> type_expr list -> unit
(* Collapse conjunctive types in class parameters *)

val get_current_level : unit -> int
val wrap_trace_gadt_instances : Env.t -> ('a -> 'b) -> 'a -> 'b
val reset_reified_var_counter : unit -> unit

val maybe_pointer_type : Env.t -> type_expr -> bool
(* True if type is possibly pointer, false if definitely not a pointer *)

(* Stubs *)
val package_subtype :
  (Env.t ->
  Path.t ->
  Longident.t list ->
  type_expr list ->
  Path.t ->
  Longident.t list ->
  type_expr list ->
  bool)
  ref

val variant_is_subtype :
  (Env.t -> Types.row_desc -> Types.type_expr -> bool) ref

val get_arity : Env.t -> type_expr -> int option
