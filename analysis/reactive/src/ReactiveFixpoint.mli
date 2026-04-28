type 'k t
(** Internal state for incremental transitive-closure computation.

    High-level model:
    - Root set [R : 'k set]
    - Edge relation [E : 'k -> 'k list]
    - Current reachable set [C : 'k set]

    Fundamental invariant:
    [C = Reach(R, E)], where [Reach] is the least fixed point of reachability
    from roots through directed edges. *)

val create : unit -> 'k t
(** Create an empty state.
    Postcondition: [R = empty], [E = empty], [C = empty]. *)

val iter_current : 'k t -> ('k -> unit -> unit) -> unit
(** Iterate keys currently in [C].
    Order is unspecified. *)

val get_current : 'k t -> 'k -> unit option
(** Membership query for [C].
    Returns [Some ()] iff the key is currently reachable, [None] otherwise. *)

val current_length : 'k t -> int
(** Cardinality of [C]. *)

val initialize :
  'k t ->
  roots_iter:(('k -> unit -> unit) -> unit) ->
  edges_iter:(('k -> 'k list -> unit) -> unit) ->
  unit
(** Replace [R] and [E] from iterators (full overwrite), then recompute closure.
    Postcondition: [C := Reach(R, E)]. *)

val apply :
  'k t ->
  init_entries:('k * unit option) list ->
  edge_entries:('k * 'k list option) list ->
  ('k * unit option) list
(** Apply one incremental update wave and return closure deltas.

    Input semantics:
    - [init_entries]: root updates, where [(k, Some ())] adds/presents root [k]
      and [(k, None)] removes root [k].
    - [edge_entries]: outgoing-edge updates, where [(k, Some succs)] sets
      [E(k) := succs] and [(k, None)] removes [k]'s edge entry.

    Correctness postcondition:
    - Let pre-state be [(R0, E0, C0)] and post-state [(R1, E1, C1)] after the
      updates. Then [C1 = Reach(R1, E1)].
    - Returned entries encode the set delta [C0 -> C1]:
      [(k, Some ())] iff [k in (C1 \\ C0)],
      [(k, None)] iff [k in (C0 \\ C1)].

    Net-effect rule:
    - If a key is tentatively deleted and rederived within the same wave, no
      remove/add pair is emitted for that key.

    Notes:
    - Output entry order is unspecified.
    - Callers should provide at most one update per key per call (or
      deduplicate before calling). *)
