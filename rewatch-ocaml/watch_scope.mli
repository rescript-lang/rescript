(** A watch scope records both known sources and unresolved dependency
    candidates. Missing candidates stay in the scope so installing or repairing
    a dependency can wake the watcher without restarting it. *)

type source_root = {
  directory: string;
  recursive: bool;
  filter: Source_filter.t option;
}

type t = {
  roots: string list;
  paths: Native_watcher.watch_path list;
  sources: source_root list;
  unresolved: string list;
}

val control_file_names : string list
val is_control_file_name : string -> bool

val discover :
  root:string ->
  prod:bool ->
  features:string list option ->
  filter:Source_filter.t option ->
  t

val path_in_scope : t -> string -> bool
val path_is_in_source_tree : t -> string -> bool
val path_is_source_ancestor : t -> string -> bool
val path_is_unresolved_ancestor : t -> string -> bool
