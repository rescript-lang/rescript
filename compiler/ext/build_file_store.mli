(** Files read and written by the compiler requests of one build.

    A build system that runs compiler requests in its own process enables the
    store for the duration of a build. Interfaces, [.cmj] files, ASTs and
    sources are then read from disk at most once per build, and a file written
    through the store is kept, so a later request, or the build system
    publishing the outputs, reads it from memory instead of from disk.

    Paths are absolute, or resolved by the caller. Contents are immutable
    strings, which domains can share. While the store is disabled, which is
    the default and the case in [bsc], every function goes to the file
    system. *)

val with_build : (unit -> 'a) -> 'a
(** [with_build f] enables an empty store while [f] runs, and empties and
    disables it afterwards. A build starts with an empty store, so that it
    sees every change made outside the store before it. *)

val enabled : unit -> bool

val generation : unit -> int
(** Changes whenever the store is emptied, so caches derived from its contents
    know to start over. *)

val read : ?keep:bool -> string -> string option
(** [read path] is the contents of [path], or [None] when it does not exist.
    Other failures raise [Sys_error]. Contents read from disk are kept for
    later reads unless [keep] is [false], which suits large files that one
    request reads, such as typed trees. *)

val exists : string -> bool

val write : ?keep:bool -> string -> string -> unit
(** [write path contents] writes [contents] to [path] and, unless [keep] is
    [false], keeps them for later reads. [keep:false] suits outputs nothing in
    the build reads again. Raises [Sys_error] like [open_out_bin]. *)

val evict : string -> unit
(** [evict path] drops the kept contents of [path], to bound memory once
    nothing in the build reads it again. Its existence is still known. *)

val set_disk_reader : (string -> string option) -> unit
(** Replaces how files missing from the store are read. The default uses
    [open_in_bin]; a build system can install one that tells a missing file
    from an unreadable one more precisely. Install it before requests run. *)

val set_held_writes : (string -> bool) -> unit
(** While the store is enabled, writes to the paths the predicate selects are
    held in memory instead of written to disk: reads see them, and
    {!publish_held} or {!forget} ends them. A build system holds interfaces
    this way, so that one is written to disk only after its compile succeeded,
    and only when it changed. Install it before requests run. *)

val publish_held :
  if_changed:bool -> write:(string -> string -> unit) -> string -> bool
(** [publish_held ~if_changed ~write path] calls [write path contents] with the
    held contents of [path], keeps them for later reads, and returns whether it
    wrote them. With [if_changed], it writes them only when they differ from
    the file on disk, so that an unchanged file keeps its modification time.
    Raises [Not_found] when nothing is held for [path]. *)

val forget : string -> unit
(** [forget path] drops everything known about [path]. Call it after changing
    [path] other than through {!write}. *)

val cached : string -> string option
(** [cached path] is the kept contents of [path], without reading the disk. *)
