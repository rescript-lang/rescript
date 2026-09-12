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

let namespace_map_key package_root = "\000namespace:" ^ package_root

type parse_message = Parse_warning of string | Parse_error of string

let has_parse_error messages =
  List.exists
    (function
      | Parse_error _ -> true
      | Parse_warning _ -> false)
    messages

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

type source_reference = {
  package_root: string;
  module_: Source.module_;
  relative_path: string;
  absolute_path: string;
}

type retained = {
  active_features: (string, string list option) Hashtbl.t;
  global_modules: (string, global_module) Hashtbl.t;
  namespace_maps: (string, namespace_map) Hashtbl.t;
  namespace_maps_by_name: (string, namespace_map list) Hashtbl.t;
  mutable graph_has_cycle: bool;
  graph_packages: (string, graph_package) Hashtbl.t;
  source_index: (string, source_reference) Hashtbl.t;
  pending_parse_paths: (string, unit) Hashtbl.t;
  cleanup_results: (string, Build_artifacts.cleanup_result) Hashtbl.t;
  mutable preparation: preparation;
  warning_state: Warning_state.t;
}

type cleanup_lifecycle = {
  mutable actions: (unit -> unit) list;
  mutable artifacts: string list;
}

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
    namespace_jobs = [];
    compile_candidates = [];
    cleanup_lifecycle = {actions = []; artifacts = []};
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

let find_active_features stats root =
  Hashtbl.find_opt stats.retained.active_features root

let set_active_features stats root features =
  Hashtbl.replace stats.retained.active_features root features

let find_global_module stats key =
  Hashtbl.find_opt stats.retained.global_modules key

let add_global_module stats key module_ =
  Hashtbl.add stats.retained.global_modules key module_

let global_module_values stats =
  Hashtbl.to_seq_values stats.retained.global_modules |> List.of_seq

let find_namespace_maps stats name =
  Hashtbl.find_opt stats.retained.namespace_maps_by_name name

let add_namespace_map stats namespace_map =
  Hashtbl.add stats.retained.namespace_maps namespace_map.key namespace_map;
  let existing =
    find_namespace_maps stats namespace_map.namespace
    |> Option.value ~default:[]
  in
  Hashtbl.replace stats.retained.namespace_maps_by_name namespace_map.namespace
    (namespace_map :: existing)

let find_namespace_map stats key =
  Hashtbl.find stats.retained.namespace_maps key

let namespace_map_values stats =
  Hashtbl.to_seq_values stats.retained.namespace_maps |> List.of_seq
let graph_has_cycle stats = stats.retained.graph_has_cycle
let set_graph_has_cycle stats value = stats.retained.graph_has_cycle <- value

let add_graph_package stats package =
  Hashtbl.replace stats.retained.graph_packages package.graph_root package

let find_graph_package stats root =
  Hashtbl.find_opt stats.retained.graph_packages root

let iter_graph_packages stats f = Hashtbl.iter f stats.retained.graph_packages

let graph_package_values stats =
  Hashtbl.to_seq_values stats.retained.graph_packages

let add_source_reference stats normalized_path source =
  Hashtbl.replace stats.retained.source_index normalized_path source

let find_source_reference stats normalized_path =
  Hashtbl.find_opt stats.retained.source_index normalized_path

let pending_parse_paths stats =
  stats.retained.pending_parse_paths |> Hashtbl.to_seq_keys |> List.of_seq

let mark_parse_pending stats path =
  Hashtbl.replace stats.retained.pending_parse_paths path ()

let clear_parse_pending stats path =
  Hashtbl.remove stats.retained.pending_parse_paths path

let set_cleanup_result stats root result =
  Hashtbl.replace stats.retained.cleanup_results root result

let find_cleanup_result stats root =
  Hashtbl.find_opt stats.retained.cleanup_results root

let warning_state stats = stats.retained.warning_state

let register_cleanup stats action =
  let cleanup = stats.cleanup_lifecycle in
  cleanup.actions <- action :: cleanup.actions

let defer_artifact_cleanup stats paths =
  let cleanup = stats.cleanup_lifecycle in
  cleanup.artifacts <- paths @ cleanup.artifacts

let take_cleanup stats =
  let cleanup = stats.cleanup_lifecycle in
  let batch = {actions = cleanup.actions; artifacts = cleanup.artifacts} in
  cleanup.actions <- [];
  cleanup.artifacts <- [];
  batch
