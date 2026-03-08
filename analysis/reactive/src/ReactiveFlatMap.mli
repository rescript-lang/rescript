(** Zero-allocation (steady-state) flatMap state and processing logic.

    This module is used by {!Reactive.FlatMap.create}. *)

type ('k1, 'v1, 'k2, 'v2) t

type process_result = {
  mutable entries_received: int;
  mutable adds_received: int;
  mutable removes_received: int;
  mutable entries_emitted: int;
  mutable adds_emitted: int;
  mutable removes_emitted: int;
}

val create :
  f:('k1 -> 'v1 -> ('k2 -> 'v2 -> unit) -> unit) ->
  merge:('v2 -> 'v2 -> 'v2) ->
  ('k1, 'v1, 'k2, 'v2) t

val destroy : ('k1, 'v1, 'k2, 'v2) t -> unit
(** Release flatMap-owned stable storage. The state must not be used
    afterwards. *)

val output_wave : ('k1, 'v1, 'k2, 'v2) t -> ('k2, 'v2 Maybe.t) StableWave.t
(** The owned output wave populated by [process]. *)

val push :
  ('k1, 'v1, 'k2, 'v2) t -> 'k1 Stable.t -> 'v1 Maybe.t Stable.t -> unit
(** Push an entry into the scratch table. *)

val process : ('k1, 'v1, 'k2, 'v2) t -> process_result
(** Process accumulated scratch entries, update target, populate output wave.
    Returns stats for the caller to apply. The output wave is populated
    (and can be sent to subscribers) only when [entries_emitted > 0]. *)

val init_entry : ('k1, 'v1, 'k2, 'v2) t -> 'k1 Stable.t -> 'v1 Stable.t -> unit
(** Initialize from an existing source entry (during setup). *)

val iter_target :
  ('k2 Stable.t -> 'v2 Stable.t -> unit) -> ('k1, 'v1, 'k2, 'v2) t -> unit
val find_target : ('k1, 'v1, 'k2, 'v2) t -> 'k2 Stable.t -> 'v2 Stable.t Maybe.t
val target_length : ('k1, 'v1, 'k2, 'v2) t -> int
