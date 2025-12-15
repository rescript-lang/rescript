(** CMT file cache using Marshal_cache for efficient mmap-based reading.
    
    This module provides cached reading of CMT files with automatic
    invalidation when files change on disk. *)

val read_cmt : string -> Cmt_format.cmt_infos
(** Read a CMT file, using the mmap cache for efficiency. *)

val read_cmt_if_changed : string -> Cmt_format.cmt_infos option
(** Read a CMT file only if it changed since the last access.
    Returns [Some cmt_infos] if the file changed (or first access),
    [None] if the file is unchanged. *)

val clear : unit -> unit
(** Clear the CMT cache, unmapping all memory. *)

val invalidate : string -> unit
(** Invalidate a specific path in the cache. *)

type stats = {entry_count: int; mapped_bytes: int}
(** Cache statistics *)

val stats : unit -> stats
(** Get cache statistics *)
