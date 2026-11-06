val path_of_parts : string -> string list -> string
val is_directory : string -> bool
val ensure_dir : string -> unit
val open_for_reading : string -> Unix.file_descr
val read_file : string -> string
val is_regular_file : string -> bool
val digest_file : string -> Digest.t
val write_file : string -> string -> unit
val append_file : string -> string -> unit
val directory_entries : string -> string list

val write_file_atomic : ?ensure_parent:bool -> string -> string -> unit
(** Writes a file next to its destination and renames it into place. *)

val atomic_write_prefix : string
(** The name prefix of the files {!write_file_atomic} renames into place; one
    left over is from a process killed while writing. *)

val exists : string -> bool
val modification_time : string -> float option
val remove_file : string -> unit
val remove_tree : string -> unit
val remove_file_best_effort : string -> unit
val files_under : string -> string list
