(** Annotation lookup over the reactive collection of per-position annotations. *)

type t
(** Abstract annotation store *)

val of_reactive :
  (Lexing.position, File_annotations.annotated_as) Reactive.t -> t
(** Wrap the reactive collection (no copy) *)

val is_annotated_dead : t -> Lexing.position -> bool
val is_annotated_gentype_or_live : t -> Lexing.position -> bool
val is_annotated_gentype_or_dead : t -> Lexing.position -> bool
