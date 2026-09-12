type cycle_info = {
  cycle: string list;
  blocked: string list;
  nodes_by_key: (string, cycle_node) Hashtbl.t;
}

and cycle_node = {
  key: string;
  package_root: string;
  source_path: string option;
  display_name: string;
}

val validate_visible_namespaces :
  root_config:Config.t -> Build_types.graph_package list -> unit

val resolved_dependencies :
  find_module:(string -> Build_types.global_module option) ->
  find_namespace_maps:(string -> Build_types.namespace_map list option) ->
  Build_types.global_module ->
  string list

val find_cycle :
  Build_types.global_module list ->
  Build_types.namespace_map list ->
  Build_state.t ->
  cycle_info option
