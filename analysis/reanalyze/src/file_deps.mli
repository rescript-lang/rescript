(** File dependencies collected during AST processing.

    Tracks which files reference which other files. *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder

val add_file : builder -> string -> unit
(** Register a file as existing (even if it has no outgoing refs). *)

val add_dep : builder -> from_file:string -> to_file:string -> unit
(** Record that [from_file] references [to_file]. *)

(** {2 Builder extraction for reactive merge} *)

val builder_files : builder -> String_set.t
(** Get files set from builder *)

val builder_deps_to_list : builder -> (string * String_set.t) list
(** Extract all deps as a list for reactive merge *)
