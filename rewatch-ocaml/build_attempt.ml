type freshness_mode = Initialize_freshness | Reuse_freshness

type cleanup_batch = {actions: (unit -> unit) list; artifacts: string list}
type namespace_job = {job: Process.job; finish: Process.result -> unit}

type pending_work = {
  mutable namespace_jobs: namespace_job list;
  mutable compile_candidates: Compiler_scheduler.candidate list;
}

type finalization_state = {
  results: (string, Build_artifacts.cleanup_result) Hashtbl.t;
  mutable actions: (unit -> unit) list;
  mutable artifacts: string list;
  initialized_logs: (string, unit) Hashtbl.t;
}

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
  namespace_freshness: (string, float option) Hashtbl.t;
  pending_work: pending_work;
  finalization: finalization_state;
  mutable compiler_cleaned: bool;
  mutable had_warnings: bool;
  process_poll: (unit -> unit) option;
  progress: Output.Progress.t;
  verbosity: int;
}

let create ~freshness_mode ~session ~process_poll ~progress ~verbosity =
  let cleanup_results = Hashtbl.create 32 in
  Build_session.iter_public_outputs session (fun root present_public_outputs ->
      Hashtbl.add cleanup_results root
        Build_artifacts.
          {
            removed_modules = [];
            previous_ast_count = 0;
            deferred_artifacts = [];
            present_public_outputs;
          });
  {
    freshness_mode;
    session;
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
    namespace_freshness = Hashtbl.create 16;
    pending_work = {namespace_jobs = []; compile_candidates = []};
    finalization =
      {
        results = cleanup_results;
        actions = [];
        artifacts = [];
        initialized_logs = Hashtbl.create 16;
      };
    compiler_cleaned = false;
    had_warnings = false;
    process_poll;
    progress;
    verbosity;
  }

let create_full ~warning_state ~process_poll ~progress ~verbosity =
  create ~freshness_mode:Initialize_freshness
    ~session:(Build_session.create ~warning_state)
    ~process_poll ~progress ~verbosity

let create_retained ~session ~process_poll ~progress ~verbosity =
  let freshness_mode =
    if Build_session.is_ready session then Reuse_freshness
    else Initialize_freshness
  in
  create ~freshness_mode ~session ~process_poll ~progress ~verbosity

let register_cleanup attempt action =
  attempt.finalization.actions <- action :: attempt.finalization.actions

let defer_artifact_cleanup attempt paths =
  attempt.finalization.artifacts <- paths @ attempt.finalization.artifacts

let take_cleanup attempt =
  let batch =
    {
      actions = attempt.finalization.actions;
      artifacts = attempt.finalization.artifacts;
    }
  in
  attempt.finalization.actions <- [];
  attempt.finalization.artifacts <- [];
  batch

let set_cleanup_result attempt root result =
  Hashtbl.replace attempt.finalization.results root result;
  Build_session.set_public_outputs attempt.session root
    result.Build_artifacts.present_public_outputs

let find_cleanup_result attempt root =
  Hashtbl.find_opt attempt.finalization.results root

let add_namespace_job attempt job =
  attempt.pending_work.namespace_jobs <-
    job :: attempt.pending_work.namespace_jobs

let take_namespace_jobs attempt =
  let jobs = List.rev attempt.pending_work.namespace_jobs in
  attempt.pending_work.namespace_jobs <- [];
  jobs

let add_compile_candidates attempt candidates =
  attempt.pending_work.compile_candidates <-
    candidates @ attempt.pending_work.compile_candidates

let take_compile_candidates attempt =
  let candidates = attempt.pending_work.compile_candidates in
  attempt.pending_work.compile_candidates <- [];
  candidates

let mark_log_initialized attempt root =
  Hashtbl.replace attempt.finalization.initialized_logs root ()

let take_initialized_logs attempt =
  let roots =
    attempt.finalization.initialized_logs |> Hashtbl.to_seq_keys |> List.of_seq
  in
  Hashtbl.clear attempt.finalization.initialized_logs;
  roots
