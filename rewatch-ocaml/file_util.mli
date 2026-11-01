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
val copy_existing_file : string -> string -> unit
val copy_optional_existing_file : string -> string -> unit

val exists : string -> bool
val files_equal : string -> string -> bool
val copy_file_if_different : string -> string -> bool

val read_build_file : string -> string
(** Reads a build file through [Build_file_store]. A missing file raises
    [Unix.Unix_error ENOENT]. *)

val write_build_file : ?keep:bool -> string -> string -> unit
val copy_build_file : ?keep:bool -> string -> string -> unit

val copy_optional_build_file : ?keep:bool -> string -> string -> unit
(** Like [copy_build_file], but removes the destination when the source does
    not exist. *)

val copy_build_file_if_different : ?keep:bool -> string -> string -> bool
(** Copies unless the destination has the same contents, and tells whether it
    copied. *)

val modification_time : string -> float option
val remove_file : string -> unit
val remove_tree : string -> unit
val remove_file_best_effort : string -> unit
val files_under : string -> string list
