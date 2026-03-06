(** Off-heap storage for raw OCaml values.

   Main concepts:
   - A [block] is an off-heap buffer managed by the allocator.
   - A block contains a number of [slots].
   - Each slot stores one raw OCaml [value] word.
   - Block [capacity] is measured in slots; byte counts are derived from that.

   This allocator does not participate in GC scanning. Storing a heap value in
   a block is therefore only safe if the value:
   - is not in the minor heap, and
   - remains reachable through ordinary OCaml roots elsewhere.

   Immediates such as [int] are always safe to store. *)

type 'a offheap
(** A value intended to be stored in off-heap structures.

    This type does not prove safety. It marks values that are crossing the
    off-heap boundary so call sites can be audited explicitly. *)

val unsafe_to_offheap : 'a -> 'a offheap
(** Unsafely mark a value as suitable for off-heap storage. The caller must
    ensure the allocator invariants hold. *)

val to_offheap : 'a -> 'a offheap
(** Safely mark a value as suitable for off-heap storage.

    Raises [Invalid_argument] if the value is currently in the minor heap.
    Immediates are accepted. *)

val unsafe_from_offheap : 'a offheap -> 'a
(** Unsafely recover a regular OCaml value from an off-heap-marked value. *)

module Block : sig
  type t

  val create : capacity:int -> t
  (** Allocate an off-heap block of raw OCaml value slots. *)

  val destroy : t -> unit
  (** Release the block storage. The handle must not be used afterwards. *)

  val capacity : t -> int
  (** Current block size, in slots. *)

  val resize : t -> capacity:int -> unit
  (** Resize the block, preserving the prefix up to the new capacity. *)

  val get : t -> int -> 'a offheap
  (** Read a slot. The caller is responsible for keeping pointed-to values
      alive and out of the minor heap while stored off-heap. *)

  val set : t -> int -> 'a offheap -> unit
  (** Write a slot. *)

  val blit :
    src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  (** Copy a range of raw value slots between blocks. *)
end

val slot_size_bytes : int
(** Size in bytes of one stored raw OCaml value slot. *)

val live_block_count : unit -> int
(** Number of currently live allocator blocks. *)

val live_block_capacity_slots : unit -> int
(** Total payload capacity, in slots, across all live blocks. *)

val is_in_minor_heap : 'a -> bool
(** Runtime check for whether a value currently resides in the OCaml minor
    heap. Immediates return [false]. Useful for enforcing the off-heap storage
    invariant in tests and debug code, and by [to_offheap]. *)
