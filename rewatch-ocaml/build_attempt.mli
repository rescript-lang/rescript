type freshness_mode = Initialize_freshness | Reuse_freshness

type namespace_job = {job: Process.job; finish: Process.result -> unit}
type pending_work
type finalization_state

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
  removed_modules: (string, unit) Hashtbl.t;
  preliminary_parses: (string, Build_types.preliminary_parse) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  namespace_freshness: (string, float option) Hashtbl.t;
  pending_work: pending_work;
  finalization: finalization_state;
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
val set_cleanup_result : t -> string -> Build_artifacts.cleanup_result -> unit
val find_cleanup_result : t -> string -> Build_artifacts.cleanup_result option
val add_namespace_job : t -> namespace_job -> unit
val take_namespace_jobs : t -> namespace_job list
val add_compile_candidates : t -> Compiler_scheduler.candidate list -> unit
val take_compile_candidates : t -> Compiler_scheduler.candidate list
val mark_log_initialized : t -> string -> unit
val cleanup_artifacts : t -> unit
val finalize_logs : t -> unit
val finish_attempt : t -> unit
val protect : t -> (unit -> 'a) -> 'a
