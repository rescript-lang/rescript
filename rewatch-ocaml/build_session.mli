type t

val create : warning_state:Warning_state.t -> t
val is_ready : t -> bool
val prepared : t -> Build_types.prepared option
val install_prepared : t -> Build_types.prepared -> unit
val mark_freshness_initialized : t -> unit
val find_global_module : t -> string -> Build_types.global_module option
val add_global_module : t -> string -> Build_types.global_module -> unit
val global_module_values : t -> Build_types.global_module list
val find_namespace_maps : t -> string -> Build_types.namespace_map list option
val add_namespace_map : t -> Build_types.namespace_map -> unit
val find_namespace_map : t -> string -> Build_types.namespace_map
val namespace_map_values : t -> Build_types.namespace_map list
val graph_has_cycle : t -> bool
val set_graph_has_cycle : t -> bool -> unit
val add_graph_package : t -> Build_types.graph_package -> unit
val find_graph_package : t -> string -> Build_types.graph_package option
val iter_graph_packages :
  t -> (string -> Build_types.graph_package -> unit) -> unit
val graph_package_values : t -> Build_types.graph_package Seq.t
val add_source_reference : t -> string -> Build_types.source_reference -> unit
val find_source_reference : t -> string -> Build_types.source_reference option
val pending_parse_paths : t -> string list
val mark_parse_pending : t -> string -> unit
val clear_parse_pending : t -> string -> unit

val set_public_outputs : t -> string -> (string, unit) Hashtbl.t -> unit
val iter_public_outputs :
  t -> (string -> (string, unit) Hashtbl.t -> unit) -> unit

val warning_state : t -> Warning_state.t
