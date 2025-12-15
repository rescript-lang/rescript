(** CMT file cache with automatic invalidation based on file metadata.
    
    This module provides cached reading of CMT files with automatic
    invalidation when files change on disk. Uses Unix.stat to detect
    changes via mtime, size, and inode. *)

val read_cmt : string -> Cmt_format.cmt_infos
(** Read a CMT file, using the cache for efficiency.
    Re-reads from disk if file has changed. *)

val read_cmt_if_changed : string -> Cmt_format.cmt_infos option
(** Read a CMT file only if it changed since the last access.
    Returns [Some cmt_infos] if the file changed (or first access),
    [None] if the file is unchanged. *)

val clear : unit -> unit
(** Clear the CMT cache, freeing all cached data. *)

val invalidate : string -> unit
(** Invalidate a specific path in the cache. *)

type stats = {entry_count: int; mapped_bytes: int}
(** Cache statistics *)

val stats : unit -> stats
(** Get cache statistics.
    Note: mapped_bytes is always 0 (we don't track actual memory usage). *)
