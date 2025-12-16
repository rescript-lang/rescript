(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References are stored in BOTH directions:
    - refs_to: posTo -> {sources that reference it}
    - refs_from: posFrom -> {targets it references}
    
    This enables gradual migration from backward to forward algorithms. *)

(** {2 Types} *)

type t
(** Immutable references - for solver (read-only) *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder

val add_value_ref :
  builder -> posTo:Lexing.position -> posFrom:Lexing.position -> unit
(** Add a value reference. Stores in both directions. *)

val add_type_ref :
  builder -> posTo:Lexing.position -> posFrom:Lexing.position -> unit
(** Add a type reference. Stores in both directions. *)

val merge_into_builder : from:builder -> into:builder -> unit
(** Merge one builder into another. *)

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

val freeze_builder : builder -> t
(** Convert builder to immutable t. Builder should not be used after this. *)

(** {2 Builder extraction for reactive merge} *)

val builder_value_refs_to_list : builder -> (Lexing.position * PosSet.t) list
(** Extract value refs in refs_to direction (posTo -> sources) *)

val builder_type_refs_to_list : builder -> (Lexing.position * PosSet.t) list
(** Extract type refs in refs_to direction (posTo -> sources) *)

val builder_value_refs_from_list : builder -> (Lexing.position * PosSet.t) list
(** Extract value refs in refs_from direction (posFrom -> targets) *)

val builder_type_refs_from_list : builder -> (Lexing.position * PosSet.t) list
(** Extract type refs in refs_from direction (posFrom -> targets) *)

val create :
  value_refs_to:PosSet.t PosHash.t ->
  type_refs_to:PosSet.t PosHash.t ->
  value_refs_from:PosSet.t PosHash.t ->
  type_refs_from:PosSet.t PosHash.t ->
  t
(** Create a References.t from hashtables (all four directions) *)

(** {2 Read-only API - refs_to direction (for reporting)} *)

val find_value_refs : t -> Lexing.position -> PosSet.t
(** Find who value-references this position *)

val find_type_refs : t -> Lexing.position -> PosSet.t
(** Find who type-references this position *)

(** {2 Read-only API - refs_from direction (for liveness)} *)

val iter_value_refs_from : t -> (Lexing.position -> PosSet.t -> unit) -> unit
(** Iterate all value refs in refs_from direction *)

val iter_type_refs_from : t -> (Lexing.position -> PosSet.t -> unit) -> unit
(** Iterate all type refs in refs_from direction *)

(** {2 Length} *)

val value_refs_length : t -> int
val type_refs_length : t -> int
