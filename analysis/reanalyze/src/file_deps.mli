(** File dependencies collected during AST processing.
    
    Tracks which files reference which other files.
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for analysis *)

(** {2 Types} *)

type t
(** Immutable file dependencies - for analysis *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder

val add_file : builder -> string -> unit
(** Register a file as existing (even if it has no outgoing refs). *)

val add_dep : builder -> from_file:string -> to_file:string -> unit
(** Merge all builders into one immutable result. Order doesn't matter. *)

(** {2 Builder extraction for reactive merge} *)

val builder_files : builder -> File_set.t
(** Get files set from builder *)

val builder_deps_to_list : builder -> (string * File_set.t) list
(** Extract all deps as a list for reactive merge *)

(** {2 Internal types (for ReactiveMerge)} *)

module File_hash : Hashtbl.S with type key = string
(** Iterate over files in topological order (roots first, leaves last).
    Files with no incoming references are processed first. *)
