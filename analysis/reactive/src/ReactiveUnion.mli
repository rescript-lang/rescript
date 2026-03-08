(** Zero-allocation union state and processing logic.

    This is a private module used by {!Reactive.Union.create}. *)

type ('k, 'v) t

type process_result = {
  mutable entries_received: int;
  mutable adds_received: int;
  mutable removes_received: int;
  mutable entries_emitted: int;
  mutable adds_emitted: int;
  mutable removes_emitted: int;
}

val create : merge:('v -> 'v -> 'v) -> ('k, 'v) t
(** Create union state with the given merge function and an owned output wave. *)

val destroy : ('k, 'v) t -> unit
(** Release union-owned stable storage. The state must not be used
    afterwards. *)

val output_wave : ('k, 'v) t -> ('k, 'v Maybe.t) ReactiveWave.t
(** The owned output wave populated by [process]. *)

val push_left : ('k, 'v) t -> 'k Stable.t -> 'v Maybe.t Stable.t -> unit
(** Push an entry into the left scratch table. *)

val push_right : ('k, 'v) t -> 'k Stable.t -> 'v Maybe.t Stable.t -> unit
(** Push an entry into the right scratch table. *)

val process : ('k, 'v) t -> process_result
(** Process accumulated scratch entries, update target, populate output wave.
    Returns stats for the caller to apply. The output wave is populated
    (and can be sent to subscribers) only when [entries_emitted > 0]. *)

val init_left : ('k, 'v) t -> 'k -> 'v -> unit
(** Initialize a left entry (during setup, before subscriptions). *)

val init_right : ('k, 'v) t -> 'k -> 'v -> unit
(** Initialize a right entry (during setup, after left). *)

val iter_target : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val find_target : ('k, 'v) t -> 'k -> 'v Maybe.t
val target_length : ('k, 'v) t -> int
