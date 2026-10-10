(** A running child is abstract so descriptors, reader threads, the waiter, and
    the platform process handle always have one cleanup owner. Callers may wait,
    cancel, or release it, but cannot reconstruct a partially owned child. *)

type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}
type stdin_policy = Inherit_stdin | Null_stdin

exception Error of string

val succeeded : result -> bool
val status_string : Unix.process_status -> string

type completion_notifier
type 'a running

val with_lock : Mutex.t -> (unit -> 'a) -> 'a
(** Runs the action holding the mutex and releases it on exceptions too. *)

val with_completion_notifier :
  ticker_enabled:bool -> (completion_notifier -> 'a) -> 'a

val notify_completion : completion_notifier -> unit
val notifier_generation : completion_notifier -> int
val await_notification : completion_notifier -> int -> int

val launch :
  ?env:Spawn.Env.t ->
  ?stdout_chunk:(bytes -> int -> unit) ->
  ?stderr_chunk:(bytes -> int -> unit) ->
  ?stdin:stdin_policy ->
  notifier:completion_notifier ->
  'a ->
  job ->
  'a running

val wait_for_running :
  poll:(unit -> unit) ->
  completion_notifier ->
  'a running list ->
  'a running * result

val payload : 'a running -> 'a
val signal_running : 'a running list -> unit
val release_after_completion : 'a running -> unit
val terminate_running : 'a running list -> unit
val release_running : 'a running -> unit
