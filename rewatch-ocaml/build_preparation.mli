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

val run :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  filter:Source_filter.t option ->
  watch:bool ->
  stats:Build_types.t ->
  parse_step:string ->
  on_cleanup:(float -> unit) ->
  cycle_info option
