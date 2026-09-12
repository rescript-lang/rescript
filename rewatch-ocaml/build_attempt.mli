type freshness_mode = Initialize_freshness | Reuse_freshness

type cleanup_batch = {actions: (unit -> unit) list; artifacts: string list}
type namespace_job = {job: Process.job; finish: Process.result -> unit}

type t = {
  freshness_mode: freshness_mode;
  session: Build_session.t;
  mutable cleaned: int;
  mutable previous_asts: int;
  mutable parsed: int;
  mutable compiled: int;
  mutable parse_seconds: float;
  mutable parse_messages: Build_types.parse_message list;
  mutable diagnostics: string list;
  mutable failure: string option;
  removed_modules: (string, unit) Hashtbl.t;
  preliminary_parses: (string, Build_types.preliminary_parse) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  initialized_logs: (string, unit) Hashtbl.t;
  namespace_freshness: (string, float option) Hashtbl.t;
  mutable namespace_jobs: namespace_job list;
  mutable compile_candidates: Compiler_scheduler.candidate list;
  cleanup_results: (string, Build_artifacts.cleanup_result) Hashtbl.t;
  mutable cleanup_actions: (unit -> unit) list;
  mutable artifact_cleanup: string list;
  mutable compiler_cleaned: bool;
  mutable had_warnings: bool;
  process_poll: (unit -> unit) option;
  progress: Output.Progress.t;
  verbosity: int;
}

val create_full :
  warning_state:Warning_state.t ->
  process_poll:(unit -> unit) option ->
  progress:Output.Progress.t ->
  verbosity:int ->
  t

val create_retained :
  session:Build_session.t ->
  process_poll:(unit -> unit) option ->
  progress:Output.Progress.t ->
  verbosity:int ->
  t

val register_cleanup : t -> (unit -> unit) -> unit
val defer_artifact_cleanup : t -> string list -> unit
val take_cleanup : t -> cleanup_batch
val set_cleanup_result : t -> string -> Build_artifacts.cleanup_result -> unit
val find_cleanup_result : t -> string -> Build_artifacts.cleanup_result option
