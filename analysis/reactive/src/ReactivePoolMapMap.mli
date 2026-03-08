(** A map from outer keys to inner maps, backed by stable storage. *)

type ('ko, 'ki, 'v) t

val create : unit -> ('ko, 'ki, 'v) t
(** [create ()] creates an empty map-of-map. *)

val destroy : ('ko, 'ki, 'v) t -> unit
(** Destroy the outer map and all owned inner maps. *)

val replace :
  ('ko, 'ki, 'v) t -> 'ko Stable.t -> 'ki Stable.t -> 'v Stable.t -> unit
(** [replace t ko ki v] ensures an inner map for [ko], then sets [ki -> v]. *)

val remove_from_inner_and_recycle_if_empty :
  ('ko, 'ki, 'v) t -> 'ko Stable.t -> 'ki Stable.t -> unit
(** Removes [ki] from [ko]'s inner map. If it becomes empty, removes [ko],
    and destroys the inner map. No-op if [ko] is absent. *)

val drain_outer :
  ('ko, 'ki, 'v) t ->
  'ko Stable.t ->
  'a ->
  ('a -> 'ki Stable.t -> 'v Stable.t -> unit) ->
  unit
(** [drain_outer t ko ctx f] iterates [f ctx ki v] for all entries in [ko]'s
    inner map, then removes [ko] and destroys the inner map.
    No-op if [ko] is absent. *)

val find_inner_maybe :
  ('ko, 'ki, 'v) t -> 'ko Stable.t -> ('ki, 'v) StableMap.t Maybe.t
(** Zero-allocation lookup of inner map by outer key.

    The returned inner map is owned by the pool-map. It becomes invalid if the
    outer binding is later removed, [drain_outer] is called, or the whole
    structure is [destroy]ed. *)

val iter_inner_with :
  ('ko, 'ki, 'v) t ->
  'ko Stable.t ->
  'a ->
  ('a -> 'ki Stable.t -> 'v Stable.t -> unit) ->
  unit
(** [iter_inner_with t ko ctx f] calls [f ctx ki v] for [ko]'s inner map.
    No-op if [ko] is absent. *)

val inner_cardinal : ('ko, 'ki, 'v) t -> 'ko Stable.t -> int
val outer_cardinal : ('ko, 'ki, 'v) t -> int

val debug_miss_count : ('ko, 'ki, 'v) t -> int
(** Always [0] in the stable-backed implementation. Kept for diagnostics and
    allocation-test compatibility. *)
