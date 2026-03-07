(** Off-heap-marked OCaml lists.

    The list cells are ordinary OCaml heap values. This type makes the
    boundary explicit when such a list is stored in an off-heap container. *)

type 'a inner
type 'a t = 'a inner ReactiveAllocator.offheap

val unsafe_of_list : 'a list -> 'a t
(** Reinterpret a list as offheap-marked without checking. *)

val of_list : 'a list -> 'a t
(** Checked version of [unsafe_of_list]. Raises if the list is still in the
    minor heap. *)

val unsafe_of_offheap_list : 'a list ReactiveAllocator.offheap -> 'a t
(** Reinterpret an already offheap-marked list as an offheap-list value. *)

val empty : unit -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
val iter_with : ('b -> 'a -> unit) -> 'b -> 'a t -> unit
val exists : ('a -> bool) -> 'a t -> bool
val exists_with : ('b -> 'a -> bool) -> 'b -> 'a t -> bool
