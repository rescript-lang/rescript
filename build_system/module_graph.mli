(** Namespace maps are graph nodes because adding or removing a namespace
    member can invalidate consumers even when no ordinary source dependency
    name changes. The graph therefore contains both source modules and these
    synthetic nodes. *)

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

val raw_dependencies :
  ?published_dependencies:(string -> string list option) ->
  artifact_dir:string ->
  parse_failed:(string -> bool) ->
  Source.module_ ->
  string list
(** The sorted module names that a module's ASTs reference, skipping the
    source paths whose parse failed. [published_dependencies path] gives the
    dependencies of [path]'s published AST, when it is known and current;
    otherwise they are read from [path]'s AST in [artifact_dir]. *)

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
  parsed_paths:(string, unit) Hashtbl.t ->
  failed_parse_paths:(string, unit) Hashtbl.t ->
  initialized
(** [parsed_paths] holds the absolute source paths parsed successfully in
    this build, and [failed_parse_paths] those whose parse failed.
    [compile_assets] was read before they were parsed. *)

val find_cycle :
  module_node list -> namespace_map list -> Build_state.t -> cycle_info option
