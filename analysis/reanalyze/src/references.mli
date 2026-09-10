(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References are stored in refs_from direction:
    - refs_from: posFrom -> {targets it references}
    
    This is what the forward liveness algorithm needs. *)

(** {2 Types} *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder

val add_value_ref :
  builder -> pos_to:Lexing.position -> pos_from:Lexing.position -> unit
(** Add a value reference. *)

val add_type_ref :
  builder -> pos_to:Lexing.position -> pos_from:Lexing.position -> unit
(** Convert builder to immutable t. Builder should not be used after this. *)

(** {2 Builder extraction for reactive merge} *)

val builder_value_refs_from_list : builder -> (Lexing.position * Pos_set.t) list
(** Extract value refs (posFrom -> targets) *)

val builder_type_refs_from_list : builder -> (Lexing.position * Pos_set.t) list
