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
  verbosity: int;
}

let create ~warning_state ~poll ~verbosity =
  {
    cleaned = 0;
    previous_asts = 0;
    parsed = 0;
    compiled = 0;
    parse_seconds = 0.;
    diagnostics = [];
    failure = None;
    removed_modules = Hashtbl.create 16;
    forced_parse_paths = Hashtbl.create 16;
    preparse_stderr = Hashtbl.create 16;
    preparse_results = Hashtbl.create 16;
    blocked_modules = Hashtbl.create 16;
    active_features = Hashtbl.create 16;
    initialized_logs = Hashtbl.create 16;
    watch_outputs = ref [];
    watch_output_paths = Hashtbl.create 16;
    global_raw_dependencies = Hashtbl.create 64;
    global_modules = Hashtbl.create 64;
    graph_packages = Hashtbl.create 32;
    cleanup_results = Hashtbl.create 32;
    deferred_artifact_cleanup = ref [];
    namespace_jobs = ref [];
    scheduled_modules = ref [];
    compile_cleanup = ref [];
    compiler_context = None;
    compile_assets = None;
    build_state = None;
    compiler_cleaned = false;
    warning_state;
    had_warnings = false;
    poll;
    verbosity;
  }

let create_incremental ~previous ~poll ~verbosity =
  (* Each rebuild needs fresh diagnostics and pending work, while the package
     graph and artifact/module state describe the long-lived watcher session.
     Sharing only that persistent subset prevents completed cleanup actions or
     failed subprocess records from leaking into the next edit. *)
  let cleanup_results = Hashtbl.create (Hashtbl.length previous.cleanup_results) in
  Hashtbl.iter
    (fun root cleanup ->
      Hashtbl.add cleanup_results root
        Build_artifacts.
          {
            removed_modules = [];
            previous_ast_count = 0;
            deferred_artifacts = [];
            present_public_outputs = cleanup.present_public_outputs;
          })
    previous.cleanup_results;
  {
    cleaned = 0;
    previous_asts = 0;
    parsed = 0;
    compiled = 0;
    parse_seconds = 0.;
    diagnostics = [];
    failure = None;
    removed_modules = Hashtbl.create 16;
    forced_parse_paths = Hashtbl.create 16;
    preparse_stderr = Hashtbl.create 16;
    preparse_results = Hashtbl.create 16;
    blocked_modules = Hashtbl.create 16;
    active_features = previous.active_features;
    initialized_logs = Hashtbl.create 16;
    watch_outputs = ref [];
    watch_output_paths = Hashtbl.create 16;
    global_raw_dependencies = previous.global_raw_dependencies;
    global_modules = previous.global_modules;
    graph_packages = previous.graph_packages;
    cleanup_results;
    deferred_artifact_cleanup = ref [];
    namespace_jobs = ref [];
    scheduled_modules = ref [];
    compile_cleanup = ref [];
    compiler_context = previous.compiler_context;
    compile_assets = previous.compile_assets;
    build_state = previous.build_state;
    compiler_cleaned = false;
    warning_state = previous.warning_state;
    had_warnings = false;
    poll;
    verbosity;
  }
