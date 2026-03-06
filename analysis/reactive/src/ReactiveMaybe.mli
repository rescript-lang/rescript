(** Zero-allocation unboxed optional values.

    An ['a t] is either [none] (a unique sentinel) or [some v].
    Unlike [option], wrapping a value with [some] performs no allocation —
    it is a plain [Obj.repr] cast.

    {b Safety contract:} [get] must only be called after [is_some] returns
    [true]. Calling [get] on [none] is undefined behavior. *)

type 'a t

val none : 'a t
(** Unique sentinel representing the absent case. *)

val none_offheap : 'a t ReactiveAllocator.offheap
(** Off-heap-marked form of [none]. Safe because the sentinel is allocated
    outside the minor heap and kept reachable for the lifetime of the process. *)

val some : 'a -> 'a t
val is_none : 'a t -> bool
val is_some : 'a t -> bool
val unsafe_get : 'a t -> 'a
val maybe_int_to_offheap : int t -> int t ReactiveAllocator.offheap
(** Safely mark an [int] maybe value as suitable for off-heap storage. *)

val maybe_unit_to_offheap : unit t -> unit t ReactiveAllocator.offheap
(** Safely mark a [unit] maybe value as suitable for off-heap storage. *)

val to_option : 'a t -> 'a option
