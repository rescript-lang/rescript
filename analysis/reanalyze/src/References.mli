(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References are stored in refs_from direction:
    - refs_from: posFrom -> {targets it references}
    
    This is what the forward liveness algorithm needs. *)

(** {2 Types} *)

type t
(** Immutable references - for solver (read-only) *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder

val add_value_ref :
  builder -> posTo:Lexing.position -> posFrom:Lexing.position -> unit
(** Add a value reference. *)

val add_type_ref :
  builder -> posTo:Lexing.position -> posFrom:Lexing.position -> unit
(** Add a type reference. *)

val merge_into_builder : from:builder -> into:builder -> unit
(** Merge one builder into another. *)

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

val freeze_builder : builder -> t
(** Convert builder to immutable t. Builder should not be used after this. *)

(** {2 Builder extraction for reactive merge} *)

val builder_value_refs_from_list : builder -> (Lexing.position * PosSet.t) list
(** Extract value refs (posFrom -> targets) *)

val builder_type_refs_from_list : builder -> (Lexing.position * PosSet.t) list
(** Extract type refs (posFrom -> targets) *)

val create :
  value_refs_from:PosSet.t PosHash.t -> type_refs_from:PosSet.t PosHash.t -> t
(** Create a References.t from hashtables *)

(** {2 Read-only API - for liveness} *)

val iter_value_refs_from : t -> (Lexing.position -> PosSet.t -> unit) -> unit
(** Iterate all value refs *)

val iter_type_refs_from : t -> (Lexing.position -> PosSet.t -> unit) -> unit
(** Iterate all type refs *)

(** {2 Length} *)

val value_refs_from_length : t -> int
val type_refs_from_length : t -> int
