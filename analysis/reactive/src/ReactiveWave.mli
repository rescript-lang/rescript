(** A wave is a growable batch of key/value entries stored in stable
    allocator-backed storage. Its API is marked with
    [Stable.t] so call sites make the boundary explicit.
    Current callers mostly use the unsafe conversions; those call sites are the
    audit surface for later enforcing the invariant. *)

type ('k, 'v) t

val create : ?max_entries:int -> unit -> ('k, 'v) t
(** Create an empty wave with an optional initial capacity hint. The wave
    grows automatically if that capacity is exceeded. *)

val clear : ('k, 'v) t -> unit
(** Remove all entries from the wave without releasing its storage. *)

val destroy : ('k, 'v) t -> unit
(** Release the wave's stable storage. The wave must not be used after this. *)

val push : ('k, 'v) t -> 'k Stable.t -> 'v Stable.t -> unit
(** Append one stable-marked entry to the wave. Callers are currently
    responsible for establishing the stable invariant before calling. *)

val iter : ('k, 'v) t -> ('k Stable.t -> 'v Stable.t -> unit) -> unit

val iter_with :
  ('k, 'v) t -> ('a -> 'k Stable.t -> 'v Stable.t -> unit) -> 'a -> unit
(** [iter_with t f arg] calls [f arg k v] for each entry.
    Unlike [iter t (f arg)], avoids allocating a closure when [f]
    is a top-level function. Prefer this on hot paths. *)

val count : ('k, 'v) t -> int
