(** Abstraction over file dependency storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [FileDeps.t] (copied from reactive)
    - [Reactive]: Direct reactive collections (no copy, zero-cost on warm runs) *)

type t =
  | Frozen of FileDeps.t
  | Reactive of {
      files: (string, unit) Reactive.t;
      deps: (string, FileSet.t) Reactive.t;
    }  (** File deps store with exposed constructors for pattern matching *)

val of_frozen : FileDeps.t -> t
(** Wrap a frozen [FileDeps.t] *)

val of_reactive :
  files:(string, unit) Reactive.t -> deps:(string, FileSet.t) Reactive.t -> t
(** Wrap reactive collections directly *)

val get_deps : t -> string -> FileSet.t
(** Get dependencies for a file *)

val iter_deps : t -> (string -> FileSet.t -> unit) -> unit
(** Iterate over all dependencies *)

val iter_files_from_roots_to_leaves : t -> (string -> unit) -> unit
(** Iterate files in topological order (roots first) *)
