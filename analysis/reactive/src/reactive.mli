(** Reactive V2: Accumulate-then-propagate scheduler for glitch-free semantics.
    
    Key design:
    1. Nodes accumulate batch deltas (don't process immediately)
    2. Scheduler visits nodes in dependency order
    3. Each node processes accumulated deltas exactly once per wave
    
    This eliminates glitches from multi-level dependencies by construction. *)

(** {1 Deltas} *)

type ('k, 'v) delta =
  | Set of 'k * 'v
  | Remove of 'k
  | Batch of ('k * 'v option) list
      (** Batch of updates: (key, Some value) = set, (key, None) = remove *)

val set : 'k -> 'v -> 'k * 'v option
(** Create a batch entry that sets a key *)

(** {1 Statistics} *)

type stats = {
  (* Input tracking *)
  mutable deltas_received: int;
      (** Number of delta messages (Set/Remove/Batch) *)
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

(** {1 Collection Interface} *)

type ('k, 'v) t = {
  name: string;
  subscribe: (('k, 'v) delta -> unit) -> unit;
  iter: ('k -> 'v -> unit) -> unit;
  get: 'k -> 'v option;
  length: unit -> int;
  stats: stats;
  level: int;
}
(** A named reactive collection at a specific topological level *)

val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val get : ('k, 'v) t -> 'k -> 'v option
val length : ('k, 'v) t -> int
val stats : ('k, 'v) t -> stats

(** {1 Source Collection} *)

val source : name:string -> unit -> ('k, 'v) t * (('k, 'v) delta -> unit)
(** Create a named source collection.
    Returns the collection and an emit function.
    Emitting triggers propagation through the pipeline. *)

(** {1 Combinators} *)

val flat_map :
  name:string ->
  ('k1, 'v1) t ->
  f:('k1 -> 'v1 -> ('k2 * 'v2) list) ->
  ?merge:('v2 -> 'v2 -> 'v2) ->
  unit ->
  ('k2, 'v2) t
(** Transform each entry into zero or more output entries.
    Optional merge function combines values for the same output key. *)

val join :
  name:string ->
  ('k1, 'v1) t ->
  ('k2, 'v2) t ->
  key_of:('k1 -> 'v1 -> 'k2) ->
  f:('k1 -> 'v1 -> 'v2 option -> ('k3 * 'v3) list) ->
  ?merge:('v3 -> 'v3 -> 'v3) ->
  unit ->
  ('k3, 'v3) t
(** Join left collection with right collection.
    For each left entry, looks up the key in right.
    Separate left/right pending buffers ensure glitch-freedom. *)

val union :
  name:string ->
  ('k, 'v) t ->
  ('k, 'v) t ->
  ?merge:('v -> 'v -> 'v) ->
  unit ->
  ('k, 'v) t
(** Combine two collections.
    Optional merge function combines values for the same key.
    Separate left/right pending buffers ensure glitch-freedom. *)

val fixpoint :
  name:string ->
  init:('k, unit) t ->
  edges:('k, 'k list) t ->
  unit ->
  ('k, unit) t
(** Compute transitive closure.
    init: initial roots
    edges: k -> successors
    Returns: all reachable keys from roots *)

(** {1 Utilities} *)

val to_mermaid : unit -> string
(** Generate Mermaid diagram of the pipeline *)

val print_stats : unit -> unit
(** Print per-node timing statistics *)

val set_debug : bool -> unit
(** Enable or disable reactive scheduler debug output (per-wave summaries). *)

val reset : unit -> unit
(** Clear all registered nodes (for tests) *)

val reset_stats : unit -> unit
(** Reset all node statistics to zero (keeps nodes intact) *)
