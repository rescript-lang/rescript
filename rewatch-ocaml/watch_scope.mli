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
