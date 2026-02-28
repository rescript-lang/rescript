type 'k t
(** Internal state for the fixpoint transitive-closure computation. *)

val create : unit -> 'k t
(** Create an empty fixpoint state. *)

val iter_current : 'k t -> ('k -> unit -> unit) -> unit
(** Iterate current reachable keys. *)

val get_current : 'k t -> 'k -> unit option
(** Get a key from the current reachable set. *)

val current_length : 'k t -> int
(** Number of currently reachable keys. *)

val initialize :
  'k t ->
  roots_iter:(('k -> unit -> unit) -> unit) ->
  edges_iter:(('k -> 'k list -> unit) -> unit) ->
  unit
(** Replace roots/edges from iterators and compute the initial closure. *)

val apply :
  'k t ->
  init_entries:('k * unit option) list ->
  edge_entries:('k * 'k list option) list ->
  ('k * unit option) list
(** Apply root/edge updates and return emitted closure deltas.
    [(k, Some ())] means newly reachable, [(k, None)] means no longer reachable. *)
