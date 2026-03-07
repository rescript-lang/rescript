type ('k, 'v) t

val create : max_entries:int -> ('k, 'v) t
val clear : ('k, 'v) t -> unit
val push : ('k, 'v) t -> 'k -> 'v -> unit
val iter : ('k, 'v) t -> ('k -> 'v -> unit) -> unit

val iter_with : ('k, 'v) t -> ('a -> 'k -> 'v -> unit) -> 'a -> unit
(** [iter_with t f arg] calls [f arg k v] for each entry.
    Unlike [iter t (f arg)], avoids allocating a closure when [f]
    is a top-level function. Prefer this on hot paths. *)

val count : ('k, 'v) t -> int
