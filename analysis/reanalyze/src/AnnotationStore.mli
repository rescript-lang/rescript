(** Abstraction over annotation storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [FileAnnotations.t] (copied from reactive)
    - [Reactive]: Direct [Reactive.t] (no copy, zero-cost on warm runs) *)

type t
(** Abstract annotation store *)

val of_frozen : FileAnnotations.t -> t
(** Wrap a frozen [FileAnnotations.t] *)

val of_reactive :
  (Lexing.position, FileAnnotations.annotated_as) Reactive.t -> t
(** Wrap a reactive collection directly (no copy) *)

val is_annotated_dead : t -> Lexing.position -> bool
val is_annotated_gentype_or_live : t -> Lexing.position -> bool
val is_annotated_gentype_or_dead : t -> Lexing.position -> bool
