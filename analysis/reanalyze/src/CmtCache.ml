(** CMT file cache with automatic invalidation based on file metadata.
    
    This module provides cached reading of CMT files with automatic
    invalidation when files change on disk. Uses Unix.stat to detect
    changes via mtime, size, and inode. *)

type file_id = {
  mtime: float;  (** Modification time *)
  size: int;  (** File size in bytes *)
  ino: int;  (** Inode number *)
}
(** File identity for cache invalidation *)

(** Get file identity from path *)
let get_file_id path : file_id =
  let st = Unix.stat path in
  {mtime = st.Unix.st_mtime; size = st.Unix.st_size; ino = st.Unix.st_ino}

(** Check if file has changed *)
let file_changed ~old_id ~new_id =
  old_id.mtime <> new_id.mtime
  || old_id.size <> new_id.size || old_id.ino <> new_id.ino

type cache_entry = {file_id: file_id; cmt_infos: Cmt_format.cmt_infos}
(** Cache entry: file identity + cached CMT data *)

(** The cache: path -> cache_entry *)
let cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 256

(** Read a CMT file, using the cache for efficiency.
    Re-reads from disk if file has changed. *)
let read_cmt path : Cmt_format.cmt_infos =
  let new_id = get_file_id path in
  match Hashtbl.find_opt cache path with
  | Some entry when not (file_changed ~old_id:entry.file_id ~new_id) ->
    entry.cmt_infos
  | _ ->
    let cmt_infos = Cmt_format.read_cmt path in
    Hashtbl.replace cache path {file_id = new_id; cmt_infos};
    cmt_infos

(** Read a CMT file only if it changed since the last access.
    Returns [Some cmt_infos] if the file changed (or first access),
    [None] if the file is unchanged.
    
    This is the key function for incremental analysis - unchanged
    files return [None] immediately without any file reading. *)
let read_cmt_if_changed path : Cmt_format.cmt_infos option =
  let new_id = get_file_id path in
  match Hashtbl.find_opt cache path with
  | Some entry when not (file_changed ~old_id:entry.file_id ~new_id) ->
    None (* File unchanged *)
  | _ ->
    let cmt_infos = Cmt_format.read_cmt path in
    Hashtbl.replace cache path {file_id = new_id; cmt_infos};
    Some cmt_infos

(** Clear the CMT cache, freeing all cached data. *)
let clear () = Hashtbl.clear cache

(** Invalidate a specific path in the cache.
    The next read will re-load the file from disk. *)
let invalidate path = Hashtbl.remove cache path

type stats = {entry_count: int; mapped_bytes: int}
(** Cache statistics *)

(** Get cache statistics.
    Note: mapped_bytes is approximate (we don't track actual memory usage). *)
let stats () : stats = {entry_count = Hashtbl.length cache; mapped_bytes = 0}
