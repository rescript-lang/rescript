(** Declarations collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    Only DceFileProcessing should use [builder].
    The solver uses [t] which is frozen/immutable. *)

(** {2 Types} *)

type t
(** Immutable declarations - for solver (read-only) *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for DceFileProcessing only} *)

val create_builder : unit -> builder
val add : builder -> Lexing.position -> Decl.t -> unit
val find_opt_builder : builder -> Lexing.position -> Decl.t option
val replace_builder : builder -> Lexing.position -> Decl.t -> unit

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

(** {2 Read-only API for t - for solver} *)

val find_opt : t -> Lexing.position -> Decl.t option
val fold : (Lexing.position -> Decl.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (Lexing.position -> Decl.t -> unit) -> t -> unit
