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
(** Add a dependency from one file to another. *)

(** {2 Merge API} *)

val merge_into_builder : from:builder -> into:builder -> unit
(** Merge one builder into another. *)

val freeze_builder : builder -> t
(** Freeze a builder into an immutable result.
    Note: Zero-copy - caller must not mutate builder after freezing. *)

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

(** {2 Read-only API for t - for analysis} *)

val get_files : t -> FileSet.t
(** Get all files. *)

val get_deps : t -> string -> FileSet.t
(** Get files that a given file depends on. *)

val iter_deps : t -> (string -> FileSet.t -> unit) -> unit
(** Iterate over all file dependencies. *)

val file_exists : t -> string -> bool
(** Check if a file exists in the graph. *)

(** {2 Topological ordering} *)

val iter_files_from_roots_to_leaves : t -> (string -> unit) -> unit
(** Iterate over files in topological order (roots first, leaves last).
    Files with no incoming references are processed first. *)
