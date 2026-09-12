type graph_dependency = {
  declaration: Config.dependency;
  directory: string;
  kind: Package_traversal.dependency_kind;
}

type graph_package = {
  graph_root: string;
  graph_build_owner: string;
  graph_is_local: bool;
  graph_config: Config.t;
  graph_compile_config: Config.t;
  graph_build_dir: string;
  graph_ocaml_dir: string;
  graph_dependencies: Config.dependency list;
  graph_dependency_directories: graph_dependency list;
  graph_gentype_dependency_args: string list;
  graph_modules: Source.module_ list;
  graph_source_mtimes: (string, float) Hashtbl.t;
  graph_source_files: string list;
  graph_present_source_files: string list;
}

type global_module = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  namespace: Config.namespace;
  allowed_dependencies: string list;
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

type parse_message = Parse_warning of string | Parse_error of string
val has_parse_error : parse_message list -> bool
type attempt_kind = Full_attempt | Retained_attempt

type preliminary_parse =
  | Parsed_successfully of {stderr: string}
  | Parse_failed of {stdout: string; stderr: string}
  | Use_existing_ast

val preliminary_parse : Process.result -> preliminary_parse

type prepared_package = {
  regular_common_args: string list;
  development_common_args: string list;
  parse_paths: string list;
}

type prepared = {
  compiler_context: Compiler_info.context;
  compile_assets: Compile_assets.t;
  build_state: Build_state.t;
  packages: (string, prepared_package) Hashtbl.t;
}

type preparation

type source_reference = {
  package_root: string;
  module_: Source.module_;
  relative_path: string;
  absolute_path: string;
}

type retained

type cleanup_lifecycle
type cleanup_batch = {actions: (unit -> unit) list; artifacts: string list}

type t = {
  attempt_kind: attempt_kind;
  mutable cleaned: int;
  mutable previous_asts: int;
  mutable parsed: int;
  mutable compiled: int;
  mutable parse_seconds: float;
  mutable parse_messages: parse_message list;
  mutable diagnostics: string list;
  mutable failure: string option;
  removed_modules: (string, unit) Hashtbl.t;
  preliminary_parses: (string, preliminary_parse) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  initialized_logs: (string, unit) Hashtbl.t;
  namespace_freshness: (string, float option) Hashtbl.t;
  mutable namespace_jobs: (Process.job * (Process.result -> unit)) list;
  mutable compile_candidates: Compiler_scheduler.candidate list;
  cleanup_lifecycle: cleanup_lifecycle;
  mutable compiler_cleaned: bool;
  retained: retained;
  mutable had_warnings: bool;
  process_poll: (unit -> unit) option;
  progress: Output.Progress.t;
  verbosity: int;
}

val create :
  warning_state:Warning_state.t ->
  process_poll:(unit -> unit) option ->
  progress:Output.Progress.t ->
  verbosity:int ->
  t

val create_incremental :
  previous:t ->
  process_poll:(unit -> unit) option ->
  progress:Output.Progress.t ->
  verbosity:int ->
  t

val prepared_exn : t -> prepared
val prepared : t -> prepared option
val install_prepared : t -> prepared -> unit
val mark_freshness_initialized : t -> unit
val prepared_package_exn : t -> string -> prepared_package
val find_active_features : t -> string -> string list option option
val set_active_features : t -> string -> string list option -> unit
val find_global_module : t -> string -> global_module option
val add_global_module : t -> string -> global_module -> unit
val global_module_values : t -> global_module list
val find_namespace_maps : t -> string -> namespace_map list option
val add_namespace_map : t -> namespace_map -> unit
val find_namespace_map : t -> string -> namespace_map
val namespace_map_values : t -> namespace_map list
val graph_has_cycle : t -> bool
val set_graph_has_cycle : t -> bool -> unit
val add_graph_package : t -> graph_package -> unit
val find_graph_package : t -> string -> graph_package option
val iter_graph_packages : t -> (string -> graph_package -> unit) -> unit
val graph_package_values : t -> graph_package Seq.t
val add_source_reference : t -> string -> source_reference -> unit
val find_source_reference : t -> string -> source_reference option
val pending_parse_paths : t -> string list
val mark_parse_pending : t -> string -> unit
val clear_parse_pending : t -> string -> unit

val set_cleanup_result : t -> string -> Build_artifacts.cleanup_result -> unit

val find_cleanup_result : t -> string -> Build_artifacts.cleanup_result option
val warning_state : t -> Warning_state.t
val register_cleanup : t -> (unit -> unit) -> unit
val defer_artifact_cleanup : t -> string list -> unit
val take_cleanup : t -> cleanup_batch
