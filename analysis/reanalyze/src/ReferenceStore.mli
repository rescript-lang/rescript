(** Abstraction over reference storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [References.t] (copied from reactive)
    - [Reactive]: Direct reactive collections (no copy, zero-cost on warm runs)

    This eliminates the O(N) freeze step when using reactive mode. *)

type t
(** Abstract reference store *)

val of_frozen : References.t -> t
(** Wrap a frozen [References.t] *)

val of_reactive :
  value_refs:(Lexing.position, PosSet.t) Reactive.t ->
  type_refs:(Lexing.position, PosSet.t) Reactive.t ->
  type_deps:ReactiveTypeDeps.t ->
  exception_refs:ReactiveExceptionRefs.t ->
  t
(** Wrap reactive collections directly (no copy) *)

(** {2 refs_to direction (for reporting)} *)

val find_value_refs : t -> Lexing.position -> PosSet.t
(** Find who value-references this position *)

val find_type_refs : t -> Lexing.position -> PosSet.t
(** Find who type-references this position *)

val get_refs_opt : t -> References.t option
(** Get underlying References.t for Frozen stores. Returns None for Reactive. *)
