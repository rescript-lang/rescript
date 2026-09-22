type t
(** A session keeps only state that must survive from one watch build to the
    next. Per-attempt diagnostics, counters, and cleanup actions deliberately
    live in {!Build_attempt} so a failed attempt cannot leak transient state
    into its successor. *)

type prepared = {
  compiler_context: Compiler_info.context;
  compile_assets: Compile_assets.t;
  build_state: Build_state.t;
  packages: (string, Package_plan.compilation) Hashtbl.t;
}

type source_reference = {
  package_root: string;
  module_: Source.module_;
  relative_path: string;
  absolute_path: string;
}

(** Cycle results are cached because ordinary implementation edits do not
    change dependency edges. [Unknown_cycle] means that graph analysis is
    required; [Known_cycle None] means it ran and found an acyclic graph. *)
type cycle_cache =
  | Unknown_cycle
  | Known_cycle of Module_graph.cycle_info option

val create : warning_state:Warning_state.t -> t
val is_ready : t -> bool
val prepared : t -> prepared option
val install_prepared : t -> prepared -> unit
val mark_freshness_initialized : t -> unit
val find_global_module : t -> string -> Module_graph.module_node option
val add_global_module : t -> string -> Module_graph.module_node -> unit
val global_module_values : t -> Module_graph.module_node list
val find_namespace_maps : t -> string -> Module_graph.namespace_map list option
val add_namespace_map : t -> Module_graph.namespace_map -> unit
val find_namespace_map : t -> string -> Module_graph.namespace_map
val namespace_map_values : t -> Module_graph.namespace_map list
val graph_cycle : t -> cycle_cache
val invalidate_graph_cycle : t -> unit
val set_graph_cycle : t -> Module_graph.cycle_info option -> unit
val add_package_plan : t -> Package_plan.t -> unit
val find_package_plan : t -> string -> Package_plan.t option
val iter_package_plans : t -> (string -> Package_plan.t -> unit) -> unit
val package_plan_values : t -> Package_plan.t Seq.t
val publish_compiler_info : t -> (Package_plan.t -> unit) -> unit
val add_source_reference : t -> string -> source_reference -> unit
val find_source_reference : t -> string -> source_reference option
val pending_parse_paths : t -> string list
val mark_parse_pending : t -> string -> unit
val clear_parse_pending : t -> string -> unit
val mark_module_removed : t -> string -> unit
val pending_removed_modules : t -> string list

val set_public_outputs : t -> string -> (string, unit) Hashtbl.t -> unit
val iter_public_outputs :
  t -> (string -> (string, unit) Hashtbl.t -> unit) -> unit

val warning_state : t -> Warning_state.t
