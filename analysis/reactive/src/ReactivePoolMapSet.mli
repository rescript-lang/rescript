(** A map from keys to sets, backed by stable storage. *)

type ('k, 'v) t

val create : unit -> ('k, 'v) t
(** [create ()] creates an empty map-of-set. *)

val destroy : ('k, 'v) t -> unit
(** Destroy the outer map and all owned inner sets. *)

val add : ('k, 'v) t -> 'k -> 'v -> unit
(** [add t k v] ensures a set exists for [k] and adds [v] to it. *)

val drain_key : ('k, 'v) t -> 'k -> 'a -> ('a -> 'v -> unit) -> unit
(** [drain_key t k ctx f] iterates [f ctx v] over the set for [k], then
    removes [k] from the outer map and destroys its inner set.
    No-op if [k] is absent. *)

val remove_from_set_and_recycle_if_empty : ('k, 'v) t -> 'k -> 'v -> unit
(** [remove_from_set_and_recycle_if_empty t k v] removes [v] from [k]'s set.
    If the set becomes empty, [k] is removed and its inner set destroyed.
    No-op if [k] is absent. *)

val find_inner_maybe : ('k, 'v) t -> 'k -> 'v StableSet.t Maybe.t
(** Zero-allocation lookup of the inner set by outer key.

    The returned inner set is owned by the pool-map. It becomes invalid if the
    outer binding is later removed, [clear] is called, or the whole structure is
    [destroy]ed. Prefer {!iter_inner_with} and {!exists_inner_with} when direct
    access is not needed. *)

val iter_inner_with :
  ('k, 'v) t -> 'k -> 'a -> ('a -> 'v -> unit) -> unit
(** [iter_inner_with t k ctx f] calls [f ctx v] for each element in [k]'s inner
    set. No-op if [k] is absent. *)

val exists_inner_with :
  ('k, 'v) t -> 'k -> 'a -> ('a -> 'v -> bool) -> bool
(** [exists_inner_with t k ctx f] returns [true] if [f ctx v] holds for some
    element in [k]'s inner set. Returns [false] if [k] is absent. *)

val iter_with :
  ('k, 'v) t -> 'a -> ('a -> 'k -> 'v StableSet.t -> unit) -> unit
(** [iter_with t ctx f] calls [f ctx k set] for each binding. *)

val clear : ('k, 'v) t -> unit
(** Removes all outer bindings and destroys their inner sets. *)

val cardinal : ('k, 'v) t -> int
(** Number of live entries in the outer map. *)

val debug_miss_count : ('k, 'v) t -> int
(** Always [0] in the stable-backed implementation. Kept for diagnostics and
    allocation-test compatibility. *)
