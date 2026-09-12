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

type module_node = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  namespace: Config.namespace;
  visible_packages: (string, unit) Hashtbl.t;
  mutable raw_dependencies: string list;
}

type namespace_map = {
  key: string;
  compiler_name: string;
  namespace: string;
  package_name: string;
  package_root: string;
  members: string list;
}

val namespace_map_key : string -> string

val validate_visible_namespaces :
  root_config:Config.t -> Package_plan.t list -> unit

val resolved_dependencies :
  find_module:(string -> module_node option) ->
  find_namespace_maps:(string -> namespace_map list option) ->
  module_node ->
  string list

type initialized = {
  nodes: module_node list;
  namespace_maps: namespace_map list;
  build_state: Build_state.t;
  use_existing_ast_paths: string list;
}

val initialize :
  root_config:Config.t ->
  package_plans:Package_plan.t list ->
  compile_assets:Compile_assets.t ->
  failed_parse_paths:(string, unit) Hashtbl.t ->
  initialized

val find_cycle :
  module_node list -> namespace_map list -> Build_state.t -> cycle_info option
