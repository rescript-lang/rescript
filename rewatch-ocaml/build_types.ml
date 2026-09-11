type dependency_kind = Regular_dependency | Development_dependency

type graph_dependency = {
  declaration: Config.dependency;
  directory: string;
  kind: dependency_kind;
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

let namespace_map_key package_root = "\000namespace:" ^ package_root

type parse_message = Parse_warning of string | Parse_error of string
type attempt_kind = Full_attempt | Retained_attempt

type preliminary_parse =
  | Parsed_successfully of {stderr: string}
  | Parse_failed of {stdout: string; stderr: string}
  | Use_existing_ast

let preliminary_parse result =
  if Process.succeeded result then Parsed_successfully {stderr = result.stderr}
  else Parse_failed {stdout = result.stdout; stderr = result.stderr}

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

type preparation =
  | Not_prepared
  | Freshness_pending of prepared
  | Ready of prepared

type retained = {
  active_features: (string, string list option) Hashtbl.t;
  global_modules: (string, global_module) Hashtbl.t;
  namespace_maps: (string, namespace_map) Hashtbl.t;
  namespace_maps_by_name: (string, namespace_map list) Hashtbl.t;
  mutable graph_has_cycle: bool;
  graph_packages: (string, graph_package) Hashtbl.t;
  source_index: (string, string * Source.module_ * string * string) Hashtbl.t;
  pending_parse_paths: (string, unit) Hashtbl.t;
  cleanup_results: (string, Build_artifacts.cleanup_result) Hashtbl.t;
  mutable preparation: preparation;
  warning_state: Warning_state.t;
}

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
  mutable deferred_artifact_cleanup: string list;
  mutable namespace_jobs: (Process.job * (Process.result -> unit)) list;
  mutable compile_candidates: Compiler_scheduler.candidate list;
  mutable compile_cleanup: (unit -> unit) list;
  mutable compiler_cleaned: bool;
  retained: retained;
  mutable had_warnings: bool;
  process_poll: (unit -> unit) option;
  progress: Output.Progress.t;
  verbosity: int;
}

let create_attempt ~attempt_kind ~retained ~process_poll ~progress ~verbosity =
  {
    attempt_kind;
    cleaned = 0;
    previous_asts = 0;
    parsed = 0;
    compiled = 0;
    parse_seconds = 0.;
    parse_messages = [];
    diagnostics = [];
    failure = None;
    removed_modules = Hashtbl.create 16;
    preliminary_parses = Hashtbl.create 16;
    blocked_modules = Hashtbl.create 16;
    initialized_logs = Hashtbl.create 16;
    namespace_freshness = Hashtbl.create 16;
    deferred_artifact_cleanup = [];
    namespace_jobs = [];
    compile_candidates = [];
    compile_cleanup = [];
    compiler_cleaned = false;
    retained;
    had_warnings = false;
    process_poll;
    progress;
    verbosity;
  }

let create ~warning_state ~process_poll ~progress ~verbosity =
  let retained =
    {
      active_features = Hashtbl.create 16;
      global_modules = Hashtbl.create 64;
      namespace_maps = Hashtbl.create 16;
      namespace_maps_by_name = Hashtbl.create 16;
      graph_has_cycle = false;
      graph_packages = Hashtbl.create 32;
      source_index = Hashtbl.create 64;
      pending_parse_paths = Hashtbl.create 16;
      cleanup_results = Hashtbl.create 32;
      preparation = Not_prepared;
      warning_state;
    }
  in
  create_attempt ~attempt_kind:Full_attempt ~retained ~process_poll ~progress
    ~verbosity

let create_incremental ~previous ~process_poll ~progress ~verbosity =
  (* Each rebuild needs fresh diagnostics and pending work, while the package
     graph and artifact/module state describe the long-lived watcher session.
     Sharing only that persistent subset prevents completed cleanup actions or
     failed subprocess records from leaking into the next edit. *)
  let cleanup_results =
    Hashtbl.create (Hashtbl.length previous.retained.cleanup_results)
  in
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
    previous.retained.cleanup_results;
  let attempt_kind =
    match previous.retained.preparation with
    | Ready _ -> Retained_attempt
    | Not_prepared | Freshness_pending _ -> Full_attempt
  in
  create_attempt ~attempt_kind
    ~retained:{previous.retained with cleanup_results}
    ~process_poll ~progress ~verbosity

let prepared_exn stats =
  match stats.retained.preparation with
  | Freshness_pending prepared | Ready prepared -> prepared
  | Not_prepared -> invalid_arg "build state has not been prepared"

let prepared stats =
  match stats.retained.preparation with
  | Freshness_pending prepared | Ready prepared -> Some prepared
  | Not_prepared -> None

let install_prepared stats prepared =
  stats.retained.preparation <- Freshness_pending prepared

let mark_freshness_initialized stats =
  match stats.retained.preparation with
  | Freshness_pending prepared -> stats.retained.preparation <- Ready prepared
  | Ready _ -> ()
  | Not_prepared -> invalid_arg "build state has not been prepared"

let prepared_package_exn stats root =
  let prepared = prepared_exn stats in
  match Hashtbl.find_opt prepared.packages root with
  | Some package -> package
  | None -> invalid_arg ("package has not been prepared: " ^ root)
