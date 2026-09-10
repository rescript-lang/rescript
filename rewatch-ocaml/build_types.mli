type graph_package = {
  graph_root: string;
  graph_build_owner: string;
  graph_is_local: bool;
  graph_config: Config.t;
  graph_compile_config: Config.t;
  graph_build_dir: string;
  graph_ocaml_dir: string;
  graph_dependencies: Config.dependency list;
  graph_dependency_directories: (Config.dependency * string) list;
  graph_modules: Source.module_ list;
  graph_source_mtimes: (string, float) Hashtbl.t;
  graph_source_files: string list;
}

type global_module = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  source: Source.module_;
  namespace: string option;
  namespace_entry: string option;
  allowed_dependencies: string list;
  mutable raw_dependencies: string list;
}

type t = {
  mutable cleaned: int;
  mutable previous_asts: int;
  mutable parsed: int;
  mutable compiled: int;
  mutable parse_seconds: float;
  mutable diagnostics: string list;
  mutable failure: string option;
  removed_modules: (string, unit) Hashtbl.t;
  forced_parse_paths: (string, unit) Hashtbl.t;
  preparse_stderr: (string, string) Hashtbl.t;
  preparse_results: (string, Process.result) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  active_features: (string, string list option) Hashtbl.t;
  initialized_logs: (string, unit) Hashtbl.t;
  watch_outputs: (string * string * string) list ref;
  watch_output_paths: (string, unit) Hashtbl.t;
  global_raw_dependencies: (string, string list) Hashtbl.t;
  global_modules: (string, global_module) Hashtbl.t;
  graph_packages: (string, graph_package) Hashtbl.t;
  cleanup_results: (string, Build_artifacts.cleanup_result) Hashtbl.t;
  deferred_artifact_cleanup: string list ref;
  namespace_jobs: (Process.job * (Process.result -> unit)) list ref;
  scheduled_modules: Compiler_scheduler.scheduled_module list ref;
  compile_cleanup: (unit -> unit) list ref;
  mutable compiler_context: Compiler_info.context option;
  mutable compile_assets: Compile_assets.t option;
  mutable build_state: Build_state.t option;
  mutable compiler_cleaned: bool;
  warning_state: Warning_state.t;
  mutable had_warnings: bool;
  poll: unit -> unit;
}

val create : warning_state:Warning_state.t -> poll:(unit -> unit) -> t

val create_incremental : previous:t -> poll:(unit -> unit) -> t
