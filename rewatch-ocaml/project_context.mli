exception Error of string
exception Package_error of string

val workspace_lock_root : string -> string
val dependency_path : string -> string -> string option

val require_dependency_directory :
  workspace_root:string -> string -> Config.dependency -> string

val relative_to : string -> string -> string
val is_local_dependency_canonical : workspace:string -> string -> bool
val is_local_dependency : workspace:string -> string -> bool
