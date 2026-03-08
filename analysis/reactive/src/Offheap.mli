(** Values marked for storage in off-heap containers.

    This type does not prove safety. It marks values that are crossing the
    off-heap boundary so call sites can be audited explicitly. *)

type 'a t

val unsafe_of_value : 'a -> 'a t
(** Unsafely mark a value as suitable for off-heap storage. The caller must
    ensure the off-heap invariants hold. *)

val of_value : 'a -> 'a t
(** Safely mark a value as suitable for off-heap storage.

    Raises [Invalid_argument] if the value is currently in the minor heap.
    Immediates are accepted. *)

val int : int -> int t
(** Safely mark an [int] as suitable for off-heap storage. *)

val unit : unit -> unit t
(** Safely mark [()] as suitable for off-heap storage. *)

val unsafe_to_value : 'a t -> 'a
(** Unsafely recover a regular OCaml value from an off-heap-marked value. *)
