(** Lists intended for storage in stable (C-allocated) containers.

    The list cells are ordinary OCaml heap values. The container that
    stores a [StableList.t] is responsible for the [Stable.t] wrapping. *)

type 'a t

val unsafe_of_list : 'a list -> 'a t
(** Reinterpret a list as a [StableList.t] without checking. *)

val of_list : 'a list -> 'a t
(** Checked version of [unsafe_of_list]. Raises if the list is still in the
    minor heap. *)

val to_stable : 'a t -> 'a t Stable.t
(** Safe conversion: a [StableList.t] is always in the major heap by
    construction, so wrapping it in [Stable.t] is safe. *)

val maybe_to_stable : 'a t Maybe.t -> 'a t Maybe.t Stable.t
(** Safe conversion for a [Maybe.t] containing a [StableList.t]. *)

val empty : unit -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : ('a Stable.t -> unit) -> 'a t -> unit
val iter_with : ('b -> 'a Stable.t -> unit) -> 'b -> 'a t -> unit
val exists : ('a Stable.t -> bool) -> 'a t -> bool
val exists_with : ('b -> 'a Stable.t -> bool) -> 'b -> 'a t -> bool
