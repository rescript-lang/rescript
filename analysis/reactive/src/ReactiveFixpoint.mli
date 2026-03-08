type 'k t
(** Internal state for incremental transitive-closure computation.

    This implementation uses fixed-capacity arrays allocated in [create]. *)

type 'k root_wave = ('k, unit Maybe.t) StableWave.t
type 'k edge_wave = ('k, 'k StableList.inner Maybe.t) StableWave.t
type 'k output_wave = ('k, unit Maybe.t) StableWave.t
type 'k root_snapshot = ('k, unit) StableWave.t
type 'k edge_snapshot = ('k, 'k StableList.inner) StableWave.t

val create : max_nodes:int -> max_edges:int -> 'k t
(** Create an empty state with fixed capacities.

    Raises [Invalid_argument] if capacities are not positive. *)

val destroy : 'k t -> unit
(** Release fixpoint-owned stable storage. The state must not be used
    afterwards. *)

val output_wave : 'k t -> 'k output_wave
(** The owned output wave populated by [apply_wave]. *)

val iter_current : 'k t -> ('k Stable.t -> unit Stable.t -> unit) -> unit
val get_current : 'k t -> 'k Stable.t -> unit Stable.t Maybe.t
val current_length : 'k t -> int

val initialize :
  'k t -> roots:'k root_snapshot -> edges:'k edge_snapshot -> unit
(** Replace roots and edges from snapshots (full overwrite), then recompute
    closure. *)

val apply_wave : 'k t -> roots:'k root_wave -> edges:'k edge_wave -> unit
(** Apply one incremental update wave and populate the owned output wave.

    Duplicate updates for the same key in one call are coalesced
    (last-write-wins). *)
