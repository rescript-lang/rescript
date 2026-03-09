(** Stable-marked OCaml lists.

    The list cells are ordinary OCaml heap values. This type makes the
    boundary explicit when such a list is stored in a stable container. *)

type 'a inner
type 'a t = 'a inner Stable.t

val unsafe_of_list : 'a list -> 'a t
(** Reinterpret a list as stable-marked without checking. *)

val unsafe_inner_of_list : 'a list -> 'a inner
(** Reinterpret a list as a [StableList.inner] without checking. *)

val of_list : 'a list -> 'a t
(** Checked version of [unsafe_of_list]. Raises if the list is still in the
    minor heap. *)

val of_stable_list : 'a list Stable.t -> 'a t
(** Reinterpret an already stable-marked list as a stable-list value. *)

val empty : unit -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : ('a Stable.t -> unit) -> 'a t -> unit
val iter_with : ('b -> 'a Stable.t -> unit) -> 'b -> 'a t -> unit
val exists : ('a Stable.t -> bool) -> 'a t -> bool
val exists_with : ('b -> 'a Stable.t -> bool) -> 'b -> 'a t -> bool
