type freshness_mode = Initialize_freshness | Reuse_freshness

type cleanup_batch = {actions: (unit -> unit) list; artifacts: string list}

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
  mutable namespace_jobs: (Process.job * (Process.result -> unit)) list;
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
    initialized_logs = Hashtbl.create 16;
    namespace_freshness = Hashtbl.create 16;
    namespace_jobs = [];
    compile_candidates = [];
    cleanup_results;
    cleanup_actions = [];
    artifact_cleanup = [];
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
  attempt.cleanup_actions <- action :: attempt.cleanup_actions

let defer_artifact_cleanup attempt paths =
  attempt.artifact_cleanup <- paths @ attempt.artifact_cleanup

let take_cleanup attempt =
  let batch =
    {actions = attempt.cleanup_actions; artifacts = attempt.artifact_cleanup}
  in
  attempt.cleanup_actions <- [];
  attempt.artifact_cleanup <- [];
  batch

let set_cleanup_result attempt root result =
  Hashtbl.replace attempt.cleanup_results root result;
  Build_session.set_public_outputs attempt.session root
    result.Build_artifacts.present_public_outputs

let find_cleanup_result attempt root =
  Hashtbl.find_opt attempt.cleanup_results root
