(** A map from keys to sets, with an internal pool for recycling inner sets.

    Eliminates allocation under key churn by recycling cleared inner sets. *)

type ('k, 'v) t

val create : capacity:int -> ('k, 'v) t
(** [create ~capacity] creates an empty pool map set.
    [capacity] is the initial pool capacity; the pool grows on demand. *)

val add : ('k, 'v) t -> 'k -> 'v -> unit
(** [add t k v] ensures a set exists for [k] and adds [v] to it. *)

val drain_key : ('k, 'v) t -> 'k -> 'a -> ('a -> 'v -> unit) -> unit
(** [drain_key t k ctx f] iterates [f ctx v] over the set for [k], then
    removes [k] from the outer map and recycles its inner set.
    No-op if [k] is absent. *)

val remove_from_set_and_recycle_if_empty : ('k, 'v) t -> 'k -> 'v -> unit
(** [remove_from_set_and_recycle_if_empty t k v] removes [v] from [k]'s set.
    If the set becomes empty, [k] is recycled. No-op if [k] is absent. *)

val find_maybe : ('k, 'v) t -> 'k -> 'v ReactiveHash.Set.t Maybe.t
(** Zero-allocation lookup. *)

val iter_with :
  ('k, 'v) t -> 'a -> ('a -> 'k -> 'v ReactiveHash.Set.t -> unit) -> unit
(** [iter_with t ctx f] calls [f ctx k set] for each binding. *)

val clear : ('k, 'v) t -> unit
(** Removes all outer bindings; inner sets are cleared and recycled. *)

val tighten : ('k, 'v) t -> unit
(** [tighten t] shrinks the outer map's capacity after key churn.
    Call explicitly after a batch of key removals. *)

val cardinal : ('k, 'v) t -> int
(** Number of live entries in the outer map. *)

val debug_miss_count : ('k, 'v) t -> int
(** Number of pool misses (fresh set allocations) since creation.
    Intended for diagnostics and allocation tests. *)
