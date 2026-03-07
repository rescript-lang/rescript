(** Off-heap mutable maps for reactive internals. *)

type ('k, 'v) t

val create : unit -> ('k, 'v) t
val destroy : ('k, 'v) t -> unit
val clear : ('k, 'v) t -> unit

val replace : ('k, 'v) t -> 'k Allocator.offheap -> 'v Allocator.offheap -> unit

val remove : ('k, 'v) t -> 'k Allocator.offheap -> unit

val mem : ('k, 'v) t -> 'k Allocator.offheap -> bool

val find_maybe :
  ('k, 'v) t -> 'k Allocator.offheap -> 'v Allocator.offheap Maybe.t

val iter_with :
  ('a -> 'k Allocator.offheap -> 'v Allocator.offheap -> unit) ->
  'a ->
  ('k, 'v) t ->
  unit

val iter :
  ('k Allocator.offheap -> 'v Allocator.offheap -> unit) -> ('k, 'v) t -> unit

val cardinal : ('k, 'v) t -> int
