(** Off-heap mutable maps for reactive internals. *)

type ('k, 'v) t

val create : unit -> ('k, 'v) t
val destroy : ('k, 'v) t -> unit
val clear : ('k, 'v) t -> unit

val replace : ('k, 'v) t -> 'k Offheap.t -> 'v Offheap.t -> unit

val remove : ('k, 'v) t -> 'k Offheap.t -> unit

val mem : ('k, 'v) t -> 'k Offheap.t -> bool

val find_maybe : ('k, 'v) t -> 'k Offheap.t -> 'v Offheap.t Maybe.t

val iter_with :
  ('a -> 'k Offheap.t -> 'v Offheap.t -> unit) -> 'a -> ('k, 'v) t -> unit

val iter : ('k Offheap.t -> 'v Offheap.t -> unit) -> ('k, 'v) t -> unit

val cardinal : ('k, 'v) t -> int
