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

val create :
  merge:('v -> 'v -> 'v) ->
  output_wave:('k, 'v ReactiveMaybe.t) ReactiveWave.t ->
  ('k, 'v) t
(** Create union state with the given merge function and output wave buffer. *)

val push_left :
  ('k, 'v) t ->
  'k ReactiveAllocator.offheap ->
  'v ReactiveMaybe.t ReactiveAllocator.offheap ->
  unit
(** Push an entry into the left scratch table. *)

val push_right :
  ('k, 'v) t ->
  'k ReactiveAllocator.offheap ->
  'v ReactiveMaybe.t ReactiveAllocator.offheap ->
  unit
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
val find_target : ('k, 'v) t -> 'k -> 'v ReactiveMaybe.t
val target_length : ('k, 'v) t -> int
