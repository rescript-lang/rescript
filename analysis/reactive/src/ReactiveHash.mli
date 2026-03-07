(** Zero-allocation (steady-state) open-addressing hash maps and sets.

    Uses linear probing with void/tomb sentinels and Obj for type erasure.
    After tables reach steady-state capacity, [clear] + [replace] cycles
    perform zero heap allocation. *)

module Map : sig
  type ('k, 'v) t

  val create : unit -> ('k, 'v) t
  val clear : ('k, 'v) t -> unit
  val replace : ('k, 'v) t -> 'k -> 'v -> unit
  val find_opt : ('k, 'v) t -> 'k -> 'v option
  val find : ('k, 'v) t -> 'k -> 'v
  val find_maybe : ('k, 'v) t -> 'k -> 'v Maybe.t
  val mem : ('k, 'v) t -> 'k -> bool
  val remove : ('k, 'v) t -> 'k -> unit

  val find_value_and_remove : ('k, 'v) t -> 'k -> 'v
  (** [find_value_and_remove t k] removes [k] and returns its value.
      Raises [Not_found] if [k] is absent. *)

  val tighten : ('k, 'v) t -> unit
  (** [tighten t] shrinks capacity when occupancy is low.
      Call after a batch of removals to reclaim backing-array space. *)

  val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit

  val iter_with : ('a -> 'k -> 'v -> unit) -> 'a -> ('k, 'v) t -> unit
  (** [iter_with f arg t] calls [f arg k v] for each binding.
      Unlike [iter (f arg) t], avoids allocating a closure when [f]
      is a top-level function. Prefer this on hot paths. *)

  val has_common_key : ('k, 'v1) t -> ('k, 'v2) t -> bool
  val cardinal : ('k, 'v) t -> int
end

module Set : sig
  type 'k t

  val create : unit -> 'k t
  val clear : 'k t -> unit
  val add : 'k t -> 'k -> unit
  val remove : 'k t -> 'k -> unit
  val mem : 'k t -> 'k -> bool

  val tighten : 'k t -> unit

  val iter : ('k -> unit) -> 'k t -> unit

  val iter_with : ('a -> 'k -> unit) -> 'a -> 'k t -> unit
  (** See {!Map.iter_with}. *)

  val exists : ('k -> bool) -> 'k t -> bool
  (** Returns [true] if any element satisfies the predicate.
      Stops scanning as soon as one element matches. *)

  val exists_with : ('a -> 'k -> bool) -> 'a -> 'k t -> bool
  (** [exists_with p arg t] is like [exists (p arg) t] but avoids closure
      allocation for top-level predicates. *)

  val cardinal : 'k t -> int
end
