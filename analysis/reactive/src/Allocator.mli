(** Stable storage for raw OCaml values.

   Main concepts:
   - A [block] is a stable buffer managed by the allocator.
   - A block contains a number of [slots].
   - Each slot stores one raw OCaml [value] word.
   - Block [capacity] is measured in slots; byte counts are derived from that.

   This allocator does not participate in GC scanning. Storing a heap value in
   a block is therefore only safe if the value:
   - is not in the minor heap, and
   - remains reachable through ordinary OCaml roots elsewhere.

   Immediates such as [int] are always safe to store. Use {!Stable} to mark
   values that cross into stable containers. *)

module Block : sig
  type 'a t

  val create : capacity:int -> 'a t
  (** Allocate a stable block of raw OCaml value slots. *)

  val destroy : 'a t -> unit
  (** Release the block storage. The handle must not be used afterwards. *)

  val capacity : 'a t -> int
  (** Current block size, in slots. *)

  val resize : 'a t -> capacity:int -> unit
  (** Resize the block, preserving the prefix up to the new capacity. *)

  val get : 'a t -> int -> 'a Stable.t
  (** Read a slot. The caller is responsible for keeping pointed-to values
      alive and out of the minor heap while stored stable. *)

  val set : 'a t -> int -> 'a Stable.t -> unit
  (** Write a slot. *)

  val blit :
    src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit
  (** Copy a range of raw value slots between blocks. *)
end

module Block2 : sig
  type ('a, 'x, 'y) t

  val create : capacity:int -> x0:'x -> y0:'y -> ('a, 'x, 'y) t
  (** Allocate a stable block with two typed header slots followed by
      [capacity] data slots. *)

  val destroy : ('a, 'x, 'y) t -> unit
  (** Release the block storage. The handle must not be used afterwards. *)

  val capacity : ('a, 'x, 'y) t -> int
  (** Current data capacity, in slots, excluding the two header slots. *)

  val resize : ('a, 'x, 'y) t -> capacity:int -> unit
  (** Resize the data region, preserving the two header slots and the data
      prefix up to the new capacity. *)

  val get0 : ('a, 'x, 'y) t -> 'x
  val set0 : ('a, 'x, 'y) t -> 'x -> unit
  val get1 : ('a, 'x, 'y) t -> 'y
  val set1 : ('a, 'x, 'y) t -> 'y -> unit

  val get : ('a, 'x, 'y) t -> int -> 'a Stable.t
  (** Read a data slot. *)

  val set : ('a, 'x, 'y) t -> int -> 'a Stable.t -> unit
  (** Write a data slot. *)

  val blit :
    src:('a, 'x, 'y) t ->
    src_pos:int ->
    dst:('a, 'u, 'v) t ->
    dst_pos:int ->
    len:int ->
    unit
  (** Copy data slots between blocks, excluding header slots. *)
end

val slot_size_bytes : int
(** Size in bytes of one stored raw OCaml value slot. *)

val live_block_count : unit -> int
(** Number of currently live allocator blocks. *)

val live_block_capacity_slots : unit -> int
(** Total payload capacity, in slots, across all live blocks. *)

val reset : unit -> unit
(** Release all allocator blocks.

    Intended for tests. Any existing block or wave handles become invalid after
    this call. *)

val is_in_minor_heap : 'a -> bool
(** Runtime check for whether a value currently resides in the OCaml minor
    heap. Immediates return [false]. Useful for enforcing the stable storage
    invariant in tests and debug code. *)
