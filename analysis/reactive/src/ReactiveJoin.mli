(** Zero-allocation (steady-state) join state and processing logic.

    This module is used by {!Reactive.Join.create}. *)

type ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t

type process_result = {
  mutable entries_received: int;
  mutable adds_received: int;
  mutable removes_received: int;
  mutable entries_emitted: int;
  mutable adds_emitted: int;
  mutable removes_emitted: int;
}

val create :
  key_of:('k1 -> 'v1 -> 'k2) ->
  f:('k1 -> 'v1 -> 'v2 Maybe.t -> ('k3 -> 'v3 -> unit) -> unit) ->
  merge:('v3 -> 'v3 -> 'v3) ->
  right_get:('k2 -> 'v2 Maybe.t) ->
  ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t

val destroy : ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> unit
(** Release join-owned stable storage. The state must not be used
    afterwards. *)

val output_wave :
  ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> ('k3, 'v3 Maybe.t) ReactiveWave.t
(** The owned output wave populated by [process]. *)

val push_left :
  ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t ->
  'k1 Stable.t ->
  'v1 Maybe.t Stable.t ->
  unit
(** Push an entry into the left scratch table. *)

val push_right :
  ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t ->
  'k2 Stable.t ->
  'v2 Maybe.t Stable.t ->
  unit
(** Push an entry into the right scratch table. *)

val process : ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> process_result
(** Process accumulated scratch entries, update target, populate output wave.
    Returns stats for the caller to apply. The output wave is populated
    (and can be sent to subscribers) only when [entries_emitted > 0]. *)

val init_entry : ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> 'k1 -> 'v1 -> unit
(** Initialize from an existing left source entry (during setup). *)

val iter_target :
  ('k3 -> 'v3 -> unit) -> ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> unit
val find_target : ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> 'k3 -> 'v3 Maybe.t
val target_length : ('k1, 'v1, 'k2, 'v2, 'k3, 'v3) t -> int
