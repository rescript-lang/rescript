(** Stable mutable maps for reactive internals. *)

type ('k, 'v) t

val create : unit -> ('k, 'v) t
val destroy : ('k, 'v) t -> unit
val clear : ('k, 'v) t -> unit

val replace : ('k, 'v) t -> 'k Stable.t -> 'v Stable.t -> unit

val remove : ('k, 'v) t -> 'k Stable.t -> unit

val mem : ('k, 'v) t -> 'k Stable.t -> bool

val find_maybe : ('k, 'v) t -> 'k Stable.t -> 'v Stable.t Maybe.t

val iter_with :
  ('a -> 'k Stable.t -> 'v Stable.t -> unit) -> 'a -> ('k, 'v) t -> unit

val iter_with2 :
  ('a -> 'b -> 'k Stable.t -> 'v Stable.t -> unit) ->
  'a ->
  'b ->
  ('k, 'v) t ->
  unit

val iter : ('k Stable.t -> 'v Stable.t -> unit) -> ('k, 'v) t -> unit

val cardinal : ('k, 'v) t -> int
