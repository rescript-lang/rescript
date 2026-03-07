(** Off-heap mutable sets for reactive internals.

    Elements are ordinary OCaml values. The set's backing storage lives in the
    custom allocator via {!Allocator.Block2}. *)

type 'a t

val create : unit -> 'a t
(** Create an empty set. *)

val destroy : 'a t -> unit
(** Release the set's owned off-heap storage. The set must not be used
    afterwards. *)

val clear : 'a t -> unit
(** Remove all elements while keeping the current storage. *)

val add : 'a t -> 'a Allocator.offheap -> unit
(** Add an element to the set. Re-adding an existing element is a no-op. *)

val remove : 'a t -> 'a Allocator.offheap -> unit
(** Remove an element from the set. Removing a missing element is a no-op. *)

val mem : 'a t -> 'a Allocator.offheap -> bool
(** Test whether the set contains an element. *)

val iter_with : ('b -> 'a Allocator.offheap -> unit) -> 'b -> 'a t -> unit
(** [iter_with f arg t] calls [f arg x] for each element. *)

val cardinal : 'a t -> int
(** Number of elements currently stored. *)
