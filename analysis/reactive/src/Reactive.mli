(** Reactive V2: Accumulate-then-propagate scheduler for glitch-free semantics.
    
    Key design:
    1. Nodes accumulate batch deltas (don't process immediately)
    2. Scheduler visits nodes in dependency order
    3. Each node processes accumulated deltas exactly once per wave
    
    This eliminates glitches from multi-level dependencies by construction. *)

(** {1 Waves} *)

type ('k, 'v) wave = ('k, 'v Maybe.t) ReactiveWave.t
(** Mutable wave buffer carrying batch entries *)

(** {1 Statistics} *)

type stats = {
  (* Input tracking *)
  mutable deltas_received: int;  (** Number of delta messages (Batch) *)
  mutable entries_received: int;  (** Total entries after expanding batches *)
  mutable adds_received: int;  (** Set operations received from upstream *)
  mutable removes_received: int;
      (** Remove operations received from upstream *)
  (* Processing tracking *)
  mutable process_count: int;  (** Times process() was called *)
  mutable process_time_ns: int64;  (** Total time in process() *)
  (* Output tracking *)
  mutable deltas_emitted: int;  (** Number of delta messages emitted *)
  mutable entries_emitted: int;  (** Total entries in emitted deltas *)
  mutable adds_emitted: int;  (** Set operations emitted downstream *)
  mutable removes_emitted: int;  (** Remove operations emitted downstream *)
}
(** Per-node statistics for diagnostics *)

val create_stats : unit -> stats

(** {1 Node Registry} *)

module Registry : sig
  type node_info
  (** Information about a registered node *)

  val clear : unit -> unit
  (** Clear all registered nodes *)

  val destroy_graph : unit -> unit
  (** Destroy all registered nodes, then clear the registry. *)

  val to_mermaid : unit -> string
  (** Generate a Mermaid diagram of the pipeline *)

  val print_stats : unit -> unit
  (** Print timing statistics for all nodes *)
end

(** {1 Scheduler} *)

module Scheduler : sig
  val propagate : unit -> unit
  (** Process all dirty nodes in topological order.
      Called automatically when a source emits. *)

  val is_propagating : unit -> bool
  (** Returns true if currently in a propagation wave *)

  val wave_count : unit -> int
  (** Number of propagation waves executed *)

  val reset_wave_count : unit -> unit
  (** Reset the wave counter *)
end

(** {1 Collection Interface} *)

type ('k, 'v) t = {
  name: string;
  subscribe: (('k, 'v) wave -> unit) -> unit;
  iter: ('k Stable.t -> 'v Stable.t -> unit) -> unit;
  get: 'k Stable.t -> 'v Stable.t Maybe.t;
  length: unit -> int;
  destroy: unit -> unit;
  stats: stats;
  level: int;
  node: Registry.node_info;
}
(** A named reactive collection at a specific topological level *)

val iter : ('k Stable.t -> 'v Stable.t -> unit) -> ('k, 'v) t -> unit
val get : ('k, 'v) t -> 'k Stable.t -> 'v Stable.t Maybe.t
val length : ('k, 'v) t -> int
val destroy : ('k, 'v) t -> unit
val stats : ('k, 'v) t -> stats
val level : ('k, 'v) t -> int
val name : ('k, 'v) t -> string

(** {1 Source Collection} *)

module Source : sig
  val create :
    name:string ->
    unit ->
    ('k, 'v) t * (('k, 'v Maybe.t) ReactiveWave.t -> unit)
  (** Create a named source collection.
      Returns the collection and an emit function that takes a wave.
      Each wave entry is a key with [Maybe.some v] for set
      or [Maybe.none] for remove.
      Emitting triggers propagation through the pipeline. *)
end

(** {1 Combinators} *)

module FlatMap : sig
  val create :
    name:string ->
    ('k1, 'v1) t ->
    f:('k1 -> 'v1 -> ('k2 -> 'v2 -> unit) -> unit) ->
    ?merge:('v2 -> 'v2 -> 'v2) ->
    unit ->
    ('k2, 'v2) t
  (** Transform each entry into zero or more output entries.
      Optional merge function combines values for the same output key. *)
end

module Join : sig
  val create :
    name:string ->
    ('k1, 'v1) t ->
    ('k2, 'v2) t ->
    key_of:('k1 -> 'v1 -> 'k2) ->
    f:('k1 -> 'v1 -> 'v2 Maybe.t -> ('k3 -> 'v3 -> unit) -> unit) ->
    ?merge:('v3 -> 'v3 -> 'v3) ->
    unit ->
    ('k3, 'v3) t
  (** Join left collection with right collection.
      For each left entry, looks up the key in right.
      Separate left/right pending buffers ensure glitch-freedom. *)
end

module Union : sig
  val create :
    name:string ->
    ('k, 'v) t ->
    ('k, 'v) t ->
    ?merge:('v -> 'v -> 'v) ->
    unit ->
    ('k, 'v) t
  (** Combine two collections.
      Optional merge function combines values for the same key.
      Separate left/right pending buffers ensure glitch-freedom. *)
end

module Fixpoint : sig
  val create :
    name:string ->
    init:('k, unit) t ->
    edges:('k, 'k StableList.inner) t ->
    unit ->
    ('k, unit) t
  (** Compute transitive closure.
      init: initial roots
      edges: k -> successors
      Returns: all reachable keys from roots *)
end

(** {1 Utilities} *)

val to_mermaid : unit -> string
(** Generate Mermaid diagram of the pipeline *)

val print_stats : unit -> unit
(** Print per-node timing statistics *)

val set_debug : bool -> unit
(** Enable or disable reactive scheduler debug output (per-wave summaries). *)

val reset : unit -> unit
(** Clear all registered nodes (for tests) *)

val destroy_graph : unit -> unit
(** Destroy all registered nodes, then clear the registry. *)

val reset_stats : unit -> unit
(** Reset all node statistics to zero (keeps nodes intact) *)
