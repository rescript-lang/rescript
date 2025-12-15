(** Marshal Cache

    A high-performance cache for marshalled files that keeps file contents
    memory-mapped (off-heap) and provides efficient repeated access with
    automatic invalidation when files change on disk.

    {2 Memory Model}

    There is no fixed-size memory pool. Each cached file gets its own [mmap]
    of exactly its file size:

    - {b mmap'd bytes}: Live in virtual address space (off-heap), managed by
      OS + cache LRU eviction
    - {b Unmarshalled value}: Lives in OCaml heap, managed by GC, exists only
      during callback

    Physical RAM pages are allocated on demand (first access). Under memory
    pressure, the OS can evict pages back to disk since they're file-backed.

    {2 Usage Example}

    {[
      Marshal_cache.with_unmarshalled_file "/path/to/data.marshal"
        (fun (data : my_data_type) ->
          (* Process data here - mmap stays valid for duration of callback *)
          process data
        )
    ]}

    {2 Platform Support}

    - macOS 10.13+: Fully supported
    - Linux (glibc): Fully supported
    - FreeBSD/OpenBSD: Should work (uses same mtime API as macOS)
    - Windows: Not supported (no mmap) *)

(** Exception raised for cache-related errors.
    Contains the file path and an error message. *)
exception Cache_error of string * string

(** Cache statistics. *)
type stats = {
  entry_count : int;        (** Number of files currently cached *)
  mapped_bytes : int;       (** Total bytes of memory-mapped data *)
}

(** [with_unmarshalled_file path f] calls [f] with the unmarshalled value
    from [path]. Guarantees the underlying mmap stays valid for the duration
    of [f].

    The cache automatically detects file changes via:
    - Modification time (nanosecond precision where available)
    - File size
    - Inode number (detects atomic file replacement)

    {b Type safety warning}: This function is inherently unsafe. The caller
    must ensure the type ['a] matches the actual marshalled data. Using the
    wrong type results in undefined behavior (crashes, memory corruption).
    This is equivalent to [Marshal.from_*] in terms of type safety.

    @raise Cache_error if the file cannot be read, mapped, or unmarshalled.
    @raise exn if [f] raises; the cache state remains consistent.

    {b Thread safety}: Safe to call from multiple threads/domains. The cache
    uses internal locking. The lock is released during the callback [f]. *)
val with_unmarshalled_file : string -> ('a -> 'r) -> 'r
  [@@alert unsafe "Caller must ensure the file contains data of the expected type"]

(** [with_unmarshalled_if_changed path f] is like {!with_unmarshalled_file} but
    only unmarshals if the file changed since the last access.

    Returns [Some (f data)] if the file changed (or is accessed for the first time).
    Returns [None] if the file has not changed since last access (no unmarshal occurs).

    This is the key primitive for building reactive/incremental systems:
    {[
      let my_cache = Hashtbl.create 100

      let get_result path =
        match Marshal_cache.with_unmarshalled_if_changed path process with
        | Some result ->
            Hashtbl.replace my_cache path result;
            result
        | None ->
            Hashtbl.find my_cache path  (* use cached result *)
    ]}

    @raise Cache_error if the file cannot be read, mapped, or unmarshalled.
    @raise exn if [f] raises; the cache state remains consistent. *)
val with_unmarshalled_if_changed : string -> ('a -> 'r) -> 'r option
  [@@alert unsafe "Caller must ensure the file contains data of the expected type"]

(** Remove all entries from the cache, unmapping all memory.
    Entries currently in use (during a callback) are preserved and will be
    cleaned up when their callbacks complete. *)
val clear : unit -> unit

(** [invalidate path] removes a specific path from the cache.
    No-op if the path is not cached or is currently in use. *)
val invalidate : string -> unit

(** [set_max_entries n] sets the maximum number of cached entries.
    When exceeded, least-recently-used entries are evicted.
    Default: 10000. Set to 0 for unlimited (not recommended for long-running
    processes).

    @raise Invalid_argument if [n < 0] *)
val set_max_entries : int -> unit

(** [set_max_bytes n] sets the maximum total bytes of mapped memory.
    When exceeded, least-recently-used entries are evicted.
    Default: 1GB (1073741824). Set to 0 for unlimited.

    @raise Invalid_argument if [n < 0] *)
val set_max_bytes : int -> unit

(** [stats ()] returns cache statistics.
    Useful for monitoring cache usage. *)
val stats : unit -> stats

