(** Values marked for storage in stable containers.

    This type does not prove safety. It marks values that are crossing the
    stable boundary so call sites can be audited explicitly. *)

type 'a t

val unsafe_of_value : 'a -> 'a t
(** Unsafely mark a value as suitable for stable storage. The caller must
    ensure the stable invariants hold. *)

val of_value : 'a -> 'a t
(** Safely mark a value as suitable for stable storage.

    Raises [Invalid_argument] if the value is currently in the minor heap.
    Immediates are accepted. *)

val int : int -> int t
(** Safely mark an [int] as suitable for stable storage. *)

val unit : unit t
(** [()] as a stable value. *)

val unsafe_to_value : 'a t -> 'a
(** Unsafely recover a regular OCaml value from a stable-marked value. *)
