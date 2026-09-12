type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

val decode_utf8_lossy : string -> string
val succeeded : result -> bool
val status_string : Unix.process_status -> string

type completion_notifier
type 'a running

val with_lock : Mutex.t -> (unit -> 'a) -> 'a

val with_completion_notifier :
  ticker_enabled:bool -> (completion_notifier -> 'a) -> 'a

val notify_completion : completion_notifier -> unit
val notifier_generation : completion_notifier -> int
val await_notification : completion_notifier -> int -> int

val launch :
  ?env:Spawn.Env.t ->
  ?stdout_chunk:(bytes -> int -> unit) ->
  ?stderr_chunk:(bytes -> int -> unit) ->
  ?defer_signals:bool ->
  notifier:completion_notifier ->
  'a ->
  job ->
  'a running

val wait_for_running :
  poll:(unit -> unit) ->
  ?defer_signals:bool ->
  completion_notifier ->
  'a running list ->
  ('a running * result) * Signal_restore.t

val payload : 'a running -> 'a
val pid : 'a running -> int
val signal_running : 'a running list -> unit
val await_termination : 'a running -> unit
val release : 'a running -> unit
val release_after_completion : 'a running -> unit
val terminate_running : 'a running list -> unit
val release_running : 'a running -> unit
