exception Error of string
exception Package_error of string

type dependency_context

val nearest_config_path : string -> string option

val workspace_lock_root_for : Config.t -> string
val workspace_lock_root : string -> string
val dependency_context : Config.t -> dependency_context
val dependency_is_local_canonical : dependency_context -> string -> bool

val dependency_candidates_in :
  dependency_context -> string -> string -> string list

val dependency_path_in : dependency_context -> string -> string -> string option

val dependency_path : string -> string -> string option

val require_dependency_directory :
  context:dependency_context -> string -> Config.dependency -> string

val relative_to : string -> string -> string
val relative_to_opt : string -> string -> string option
val relative_or_absolute : root:string -> string -> string
val display_path : root:string -> string -> string
val is_local_dependency_canonical : workspace:string -> string -> bool
val path_is_within_canonical : root:string -> string -> bool
