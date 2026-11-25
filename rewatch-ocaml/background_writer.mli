(** Writes build outputs on a domain of their own, so compiler workers don't
    wait for the file system. A prototype: [RESCRIPT_BACKGROUND_WRITES=1]
    turns it on; otherwise every write runs right away, as before.

    Writes run in the order they were queued. Readers in the build get the
    contents from the build file store until they are on disk, and the build
    waits for the queue with {!flush} before anything outside it reads the
    files. With [RESCRIPT_TRACE], every write is a span named after its file,
    with its size and kind, on the writer's track or within the worker's
    [publish] span. *)

val enabled : bool

val write :
  write:(path:string -> string -> unit) ->
  ?after:(unit -> unit) ->
  string ->
  string ->
  unit
(** [write ~write path contents] writes [contents] to [path] with [write],
    now or on the writer, and then runs [after]. *)

val flush : unit -> unit
(** Waits until every queued write has finished, and raises the first error of
    a write since the previous flush. *)

val flush_quietly : unit -> unit
(** {!flush}, but drops write errors, for when the build already failed. *)
