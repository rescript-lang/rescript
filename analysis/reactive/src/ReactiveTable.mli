type 'a t

val create : initial_capacity:int -> 'a t
(** Create an extensible stable table.

    Stored values are raw OCaml values kept outside the GC's scanned heap.
    This is only safe for immediates, or for heap values that are:
    1. promoted out of the minor heap, and
    2. kept reachable through normal OCaml roots elsewhere.

    Intended reactive protocol:
    1. Produce a wave of fresh OCaml values on the heap.
    2. Promote them out of the minor heap before stable publication.
    3. Insert them into stable reactive tables during the allocation-free
       processing phase.
    4. After the iteration finishes, flush/remove table entries as needed.
    5. Only then drop the ordinary OCaml roots for removed values.

    Violating this protocol is unsafe:
    - minor-heap values may move, leaving stale pointers stable
    - unrooted major-heap values may be reclaimed *)

val destroy : 'a t -> unit
(** Release the table storage. The handle must not be used afterwards. *)

val length : 'a t -> int
(** Number of elements currently stored in the table. *)

val capacity : 'a t -> int
(** Current table capacity, in elements. *)

val clear : 'a t -> unit
(** Remove all elements from the table without releasing its storage. *)

val get : 'a t -> int -> 'a Stable.t
val set : 'a t -> int -> 'a Stable.t -> unit

val push : 'a t -> 'a Stable.t -> unit
(** Append an element, growing via the allocator when needed. *)

val pop : 'a t -> 'a Stable.t
(** Remove and return the last element. *)

val shrink_to_fit : 'a t -> unit
(** Shrink storage capacity down to the current length. *)
