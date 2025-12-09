(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References track which positions reference which declarations.
    Both value references and type references are tracked. *)

(** {2 Types} *)

type t
(** Immutable references - for solver (read-only) *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder
val add_value_ref :
  builder -> posTo:Lexing.position -> posFrom:Lexing.position -> unit
val add_type_ref :
  builder -> posTo:Lexing.position -> posFrom:Lexing.position -> unit

val merge_into_builder : from:builder -> into:builder -> unit
(** Merge one builder into another. *)

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

val freeze_builder : builder -> t
(** Convert builder to immutable t. Builder should not be used after this. *)

(** {2 Read-only API for t - for solver} *)

val find_value_refs : t -> Lexing.position -> PosSet.t
val find_type_refs : t -> Lexing.position -> PosSet.t
