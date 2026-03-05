(** A map from outer keys to inner maps, with pooled recycling of inner maps.

    Designed for churn-heavy map-of-map usage where empty inner maps should be
    removed and recycled deterministically. *)

type ('ko, 'ki, 'v) t

val create : capacity:int -> ('ko, 'ki, 'v) t
(** [create ~capacity] creates an empty pooled map-of-map.
    [capacity] is the initial pool capacity; pool grows on demand. *)

val replace : ('ko, 'ki, 'v) t -> 'ko -> 'ki -> 'v -> unit
(** [replace t ko ki v] ensures an inner map for [ko], then sets [ki -> v]. *)

val remove_from_inner_and_recycle_if_empty :
  ('ko, 'ki, 'v) t -> 'ko -> 'ki -> unit
(** Removes [ki] from [ko]'s inner map. If it becomes empty, removes [ko],
    clears and recycles the inner map. No-op if [ko] is absent. *)

val drain_outer :
  ('ko, 'ki, 'v) t -> 'ko -> 'a -> ('a -> 'ki -> 'v -> unit) -> unit
(** [drain_outer t ko ctx f] iterates [f ctx ki v] for all entries in [ko]'s
    inner map, then removes [ko], clears and recycles the inner map.
    No-op if [ko] is absent. *)

val find_inner_maybe :
  ('ko, 'ki, 'v) t -> 'ko -> ('ki, 'v) ReactiveHash.Map.t ReactiveMaybe.t
(** Zero-allocation lookup of inner map by outer key. *)

val iter_inner_with :
  ('ko, 'ki, 'v) t -> 'ko -> 'a -> ('a -> 'ki -> 'v -> unit) -> unit
(** [iter_inner_with t ko ctx f] calls [f ctx ki v] for [ko]'s inner map.
    No-op if [ko] is absent. *)

val inner_cardinal : ('ko, 'ki, 'v) t -> 'ko -> int
val outer_cardinal : ('ko, 'ki, 'v) t -> int

val tighten : ('ko, 'ki, 'v) t -> unit
(** Shrinks the outer map capacity after major churn. *)

val debug_miss_count : ('ko, 'ki, 'v) t -> int
(** Number of pool misses (fresh inner-map allocations) since creation. *)
