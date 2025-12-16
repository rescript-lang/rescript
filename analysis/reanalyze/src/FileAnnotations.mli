(** Source annotations (@dead, @live, @genType).
    
    Two types are provided:
    - [builder] - mutable, for AST processing and merging
    - [t] - immutable, for solver (read-only access)
    
    Only DceFileProcessing should use [builder].
    The solver uses [t] which is frozen/immutable. *)

(** {2 Types} *)

type annotated_as = GenType | Dead | Live  (** Annotation type *)

type t
(** Immutable annotations - for solver (read-only) *)

type builder
(** Mutable builder - for AST processing and merging *)

(** {2 Builder API - for DceFileProcessing only} *)

val create_builder : unit -> builder
val annotate_gentype : builder -> Lexing.position -> unit
val annotate_dead : builder -> Lexing.position -> unit
val annotate_live : builder -> Lexing.position -> unit

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

(** {2 Builder extraction for reactive merge} *)

val builder_to_list : builder -> (Lexing.position * annotated_as) list
(** Extract all annotations as a list for reactive merge *)

val create_from_hashtbl : annotated_as PosHash.t -> t
(** Create from hashtable for reactive merge *)

(** {2 Read-only API for t - for solver} *)

val is_annotated_dead : t -> Lexing.position -> bool
val is_annotated_gentype_or_live : t -> Lexing.position -> bool
val is_annotated_gentype_or_dead : t -> Lexing.position -> bool
val length : t -> int
val iter : (Lexing.position -> annotated_as -> unit) -> t -> unit
