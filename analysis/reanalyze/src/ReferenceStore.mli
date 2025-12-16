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

val find_value_refs : t -> Lexing.position -> PosSet.t
(** Find value references to a position *)

val find_type_refs : t -> Lexing.position -> PosSet.t
(** Find type references to a position *)
