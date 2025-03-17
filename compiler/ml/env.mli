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

(* Environment handling *)

open Types

module PathMap :
  Map.S with type key = Path.t and type 'a t = 'a Map.Make(Path).t

type summary =
  | Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration PathMap.t
  | Env_copy_types of summary * string list

type t

val empty : t
val initial_safe_string : t

val diff : t -> t -> Ident.t list
val copy_local : from:t -> t -> t

type type_descriptions = constructor_description list * label_description list

(* For short-paths *)
type iter_cont
val iter_types :
  (Path.t -> Path.t * (type_declaration * type_descriptions) -> unit) ->
  t ->
  iter_cont
val run_iter_cont : iter_cont list -> (Path.t * iter_cont) list
val same_types : t -> t -> bool
val used_persistent : unit -> Concr.t
val find_shadowed_types : Path.t -> t -> Path.t list
val without_cmis : ('a -> 'b) -> 'a -> 'b
(* [without_cmis f arg] applies [f] to [arg], but does not
   allow opening cmis during its execution *)

(* Lookup by paths *)

val find_value : Path.t -> t -> value_description
val find_type : Path.t -> t -> type_declaration
val find_type_descrs : Path.t -> t -> type_descriptions
val find_module : Path.t -> t -> module_declaration
val find_modtype : Path.t -> t -> modtype_declaration

val find_type_expansion : Path.t -> t -> type_expr list * type_expr * int option
val find_type_expansion_opt :
  Path.t -> t -> type_expr list * type_expr * int option

(* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. *)
val find_modtype_expansion : Path.t -> t -> module_type
val add_functor_arg : Ident.t -> t -> t
val is_functor_arg : Path.t -> t -> bool
val normalize_path : Location.t option -> t -> Path.t -> Path.t

(* Normalize the path to a concrete value or module.
   If the option is None, allow returning dangling paths.
   Otherwise raise a Missing_module error, and may add forgotten
   head as required global. *)
val normalize_path_prefix : Location.t option -> t -> Path.t -> Path.t
(* Only normalize the prefix part of the path *)

val has_local_constraints : t -> bool
val add_gadt_instance_level : int -> t -> t
val gadt_instance_level : t -> type_expr -> int option
val add_gadt_instances : t -> int -> type_expr list -> unit
val add_gadt_instance_chain : t -> int -> type_expr -> unit

(* Lookup by long identifiers *)

(* ?loc is used to report 'deprecated module' warnings *)

val lookup_value :
  ?loc:Location.t -> Longident.t -> t -> Path.t * value_description
val lookup_constructor :
  ?loc:Location.t -> Longident.t -> t -> constructor_description
val lookup_all_constructors :
  ?loc:Location.t ->
  Longident.t ->
  t ->
  (constructor_description * (unit -> unit)) list
val lookup_label : ?loc:Location.t -> Longident.t -> t -> label_description
val lookup_all_labels :
  ?loc:Location.t ->
  Longident.t ->
  t ->
  (label_description * (unit -> unit)) list
val lookup_type : ?loc:Location.t -> Longident.t -> t -> Path.t
(* Since 4.04, this function no longer returns [type_description].
   To obtain it, you should either call [Env.find_type], or replace
   it by [Typetexp.find_type] *)

val lookup_module : load:bool -> ?loc:Location.t -> Longident.t -> t -> Path.t
val lookup_modtype :
  ?loc:Location.t -> Longident.t -> t -> Path.t * modtype_declaration

val copy_types : string list -> t -> t
(* Used only in Typecore.duplicate_ident_types. *)

exception Recmodule
(* Raise by lookup_module when the identifier refers
   to one of the modules of a recursive definition
   during the computation of its approximation (see #5965). *)

(* Insertion by identifier *)

val add_value :
  ?check:(string -> Warnings.t) -> Ident.t -> value_description -> t -> t
val add_type : check:bool -> Ident.t -> type_declaration -> t -> t
val add_extension : check:bool -> Ident.t -> extension_constructor -> t -> t
val add_module : ?arg:bool -> Ident.t -> module_type -> t -> t
val add_module_declaration :
  ?arg:bool -> check:bool -> Ident.t -> module_declaration -> t -> t
val add_modtype : Ident.t -> modtype_declaration -> t -> t

val add_local_constraint : Path.t -> type_declaration -> int -> t -> t
val add_local_type : Path.t -> type_declaration -> t -> t

(* Insertion of all fields of a signature. *)

val add_item : signature_item -> t -> t
val add_signature : signature -> t -> t

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. Returns None if the path refers to a functor,
   not a structure. *)
val open_signature :
  ?used_slot:bool ref ->
  ?loc:Location.t ->
  ?toplevel:bool ->
  Asttypes.override_flag ->
  Path.t ->
  t ->
  t option

val open_pers_signature : string -> t -> t

(* Insertion by name *)

val enter_value :
  ?check:(string -> Warnings.t) ->
  string ->
  value_description ->
  t ->
  Ident.t * t
val enter_type : string -> type_declaration -> t -> Ident.t * t
val enter_extension : string -> extension_constructor -> t -> Ident.t * t
val enter_module : ?arg:bool -> string -> module_type -> t -> Ident.t * t
val enter_module_declaration :
  ?arg:bool -> Ident.t -> module_declaration -> t -> t
val enter_modtype : string -> modtype_declaration -> t -> Ident.t * t

(* Initialize the cache of in-core module interfaces. *)
val reset_cache : unit -> unit

(* To be called before each toplevel phrase. *)
val reset_cache_toplevel : unit -> unit

(* Remember the name of the current compilation unit. *)
val set_unit_name : string -> unit
val get_unit_name : unit -> string

(* Read, save a signature to/from a file *)

val read_signature : string -> string -> signature
(* Arguments: module name, file name. Results: signature. *)

val save_signature :
  ?check_exists:unit ->
  deprecated:string option ->
  signature ->
  string ->
  string ->
  Cmi_format.cmi_infos
(* Arguments: signature, module name, file name. *)

val save_signature_with_imports :
  ?check_exists:unit ->
  deprecated:string option ->
  signature ->
  string ->
  string ->
  (string * Digest.t option) list ->
  Cmi_format.cmi_infos
(* Arguments: signature, module name, file name,
   imported units with their CRCs. *)

(* Return the CRC of the interface of the given compilation unit *)

val crc_of_unit : string -> Digest.t

(* Return the set of compilation units imported, with their CRC *)

val imports : unit -> (string * Digest.t option) list

(* Direct access to the table of imported compilation units with their CRC *)

val crc_units : Consistbl.t
val add_import : string -> unit

(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)

val summary : t -> summary

(* Return an equivalent environment where all fields have been reset,
   except the summary. The initial environment can be rebuilt from the
   summary, using Envaux.env_of_only_summary. *)

val keep_only_summary : t -> t
val env_of_only_summary : (summary -> Subst.t -> t) -> t -> t

(* Error report *)

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string

exception Error of error

open Format

val report_error : formatter -> error -> unit

val mark_value_used : t -> string -> value_description -> unit
val mark_module_used : t -> string -> Location.t -> unit
val mark_type_used : t -> string -> type_declaration -> unit

type constructor_usage = Positive | Pattern | Privatize
val mark_constructor_used :
  constructor_usage -> t -> string -> type_declaration -> string -> unit
val mark_constructor :
  constructor_usage -> t -> string -> constructor_description -> unit
val mark_extension_used :
  constructor_usage -> t -> extension_constructor -> string -> unit

val in_signature : bool -> t -> t
val implicit_coercion : t -> t

val is_in_signature : t -> bool

val set_value_used_callback :
  string -> value_description -> (unit -> unit) -> unit
val set_type_used_callback :
  string -> type_declaration -> ((unit -> unit) -> unit) -> unit

(* Forward declaration to break mutual recursion with Includemod. *)
val check_modtype_inclusion :
  (loc:Location.t -> t -> module_type -> Path.t -> module_type -> unit) ref

(* Forward declaration to break mutual recursion with Mtype. *)
val strengthen :
  (aliasable:bool -> t -> module_type -> Path.t -> module_type) ref

(* Forward declaration to break mutual recursion with Ctype. *)
val same_constr : (t -> type_expr -> type_expr -> bool) ref

(** Folding over all identifiers (for analysis purpose) *)

val fold_values :
  (string -> Path.t -> value_description -> 'a -> 'a) ->
  Longident.t option ->
  t ->
  'a ->
  'a
val fold_types :
  (string -> Path.t -> type_declaration * type_descriptions -> 'a -> 'a) ->
  Longident.t option ->
  t ->
  'a ->
  'a
val fold_constructors :
  (constructor_description -> 'a -> 'a) -> Longident.t option -> t -> 'a -> 'a
val fold_labels :
  (label_description -> 'a -> 'a) -> Longident.t option -> t -> 'a -> 'a

val fold_modules :
  (string -> Path.t -> module_declaration -> 'a -> 'a) ->
  Longident.t option ->
  t ->
  'a ->
  'a
(** Persistent structures are only traversed if they are already loaded. *)

val fold_modtypes :
  (string -> Path.t -> modtype_declaration -> 'a -> 'a) ->
  Longident.t option ->
  t ->
  'a ->
  'a

val scrape_alias : t -> module_type -> module_type
(** Utilities *)

val check_value_name : string -> Location.t -> unit

module Persistent_signature : sig
  type t = {
    filename: string;  (** Name of the file containing the signature. *)
    cmi: Cmi_format.cmi_infos;
  }

  val load : (unit_name:string -> t option) ref
  (** Function used to load a persistent signature. The default is to look for
      the .cmi file in the load path. This function can be overridden to load
      it from memory, for instance to build a self-contained toplevel. *)
end
