val path_of_parts : string -> string list -> string
val ensure_dir : string -> unit
val read_file : string -> string
val write_file : string -> string -> unit
val append_file : string -> string -> unit

val write_file_atomic :
  ?ensure_parent:bool -> ?perm:int -> string -> string -> unit

val copy_existing_file : ?ensure_parent:bool -> string -> string -> unit

val copy_optional_existing_file :
  ?ensure_parent:bool -> string -> string -> unit

val copy_file : string -> string -> unit
val files_equal : string -> string -> bool
val copy_file_if_changed : ?ensure_parent:bool -> string -> string -> unit
val modification_time : string -> float option
val remove_file : string -> unit
val remove_tree : string -> unit
val remove_file_best_effort : string -> unit
val files_under : string -> string list
