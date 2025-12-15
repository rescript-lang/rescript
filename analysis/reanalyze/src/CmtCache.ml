(** CMT file cache using Marshal_cache for efficient mmap-based reading.
    
    This module provides cached reading of CMT files with automatic
    invalidation when files change on disk. It's used to speed up
    repeated analysis runs by avoiding re-reading unchanged files. *)

[@@@alert "-unsafe"]

(** Read a CMT file, using the mmap cache for efficiency.
    The file is memory-mapped and the cache automatically detects
    when the file changes on disk. *)
let read_cmt path : Cmt_format.cmt_infos =
  Marshal_cache.with_unmarshalled_file path Fun.id

(** Read a CMT file only if it changed since the last access.
    Returns [Some cmt_infos] if the file changed (or first access),
    [None] if the file is unchanged.
    
    This is the key function for incremental analysis - unchanged
    files return [None] immediately without any unmarshalling. *)
let read_cmt_if_changed path : Cmt_format.cmt_infos option =
  Marshal_cache.with_unmarshalled_if_changed path Fun.id

(** Clear the CMT cache, unmapping all memory.
    Useful for testing or to free memory. *)
let clear () = Marshal_cache.clear ()

(** Invalidate a specific path in the cache.
    The next read will re-load the file from disk. *)
let invalidate path = Marshal_cache.invalidate path

(** Cache statistics *)
type stats = {
  entry_count: int;
  mapped_bytes: int;
}

(** Get cache statistics *)
let stats () : stats =
  let s = Marshal_cache.stats () in
  { entry_count = s.entry_count; mapped_bytes = s.mapped_bytes }

