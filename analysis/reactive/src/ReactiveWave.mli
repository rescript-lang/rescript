(** A wave is a growable batch of key/value entries stored in off-heap
    allocator-backed storage. Its API is marked with
    [ReactiveAllocator.offheap] so call sites make the boundary explicit.
    Current callers mostly use the unsafe conversions; those call sites are the
    audit surface for later enforcing the invariant. *)

type ('k, 'v) t

val create : max_entries:int -> ('k, 'v) t
(** Create an empty wave with an initial capacity hint. The wave grows
    automatically if that capacity is exceeded. *)

val clear : ('k, 'v) t -> unit
(** Remove all entries from the wave without releasing its storage. *)

val destroy : ('k, 'v) t -> unit
(** Release the wave's off-heap storage. The wave must not be used after this. *)

val push :
  ('k, 'v) t ->
  'k ReactiveAllocator.offheap ->
  'v ReactiveAllocator.offheap ->
  unit
(** Append one off-heap-marked entry to the wave. Callers are currently
    responsible for establishing the off-heap invariant before calling. *)

val iter :
  ('k, 'v) t ->
  ('k ReactiveAllocator.offheap -> 'v ReactiveAllocator.offheap -> unit) ->
  unit

val iter_with :
  ('k, 'v) t ->
  ('a -> 'k ReactiveAllocator.offheap -> 'v ReactiveAllocator.offheap -> unit) ->
  'a ->
  unit
(** [iter_with t f arg] calls [f arg k v] for each entry.
    Unlike [iter t (f arg)], avoids allocating a closure when [f]
    is a top-level function. Prefer this on hot paths. *)

val count : ('k, 'v) t -> int
