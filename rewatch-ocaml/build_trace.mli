(** The build trace. With [RESCRIPT_TRACE=<file>], the build records what each
    domain does and writes it to the file when the process exits, in the
    Chrome trace event format, which Perfetto (ui.perfetto.dev) and
    [chrome://tracing] open as a timeline:

    - a track for the main domain, with a span per dependency graph and per
      scheduler step (starting a task, taking a result), and counters for the
      tasks that are ready and running;
    - a track per worker slot, with a span per compiler request, named after
      its input file, that holds the request's allocations and the garbage
      collections during it as arguments, and, for implementations, child
      spans for type checking and code generation; after it, a [publish] span
      for the task's result handler, which publishes what the request
      wrote.

    Times are whole microseconds. A command that records nothing leaves the
    file alone.

    The format is meant for people and may change. Without the variable,
    [enabled] is false and nothing is recorded. *)

val enabled : bool

val now : unit -> float
(** Seconds, for the [start] of {!span}. *)

val set_worker_slot : int -> unit
(** Puts the current domain's spans on the track of worker [slot] (from 0).
    Domains that never call it use the main track. *)

val span :
  name:string ->
  ?args:(string * Yojson.Basic.t) list ->
  start:float ->
  unit ->
  unit
(** Records a span on the current domain's track from [start] until now. *)

val counters : name:string -> (string * int) list -> unit
(** Records the current values of a group of counters. *)

val compiler_request : phase:string -> input:string -> (unit -> 'a) -> 'a
(** Runs a compiler request in a span. *)
