type freshness_mode = Initialize_freshness | Reuse_freshness

type parse_message = Parse_warning of string | Parse_error of string

let has_parse_error messages =
  List.exists
    (function
      | Parse_error _ -> true
      | Parse_warning _ -> false)
    messages

type preliminary_parse =
  | Parsed_successfully of {stderr: string}
  | Parse_failed of {stdout: string; stderr: string}
  | Use_existing_ast

let preliminary_parse result =
  if Process.succeeded result then Parsed_successfully {stderr = result.stderr}
  else Parse_failed {stdout = result.stdout; stderr = result.stderr}

type namespace_job = {job: Process.job; finish: Process.result -> unit}

type pending_work = {
  mutable namespace_jobs: namespace_job list;
  mutable compile_candidates: Compiler_scheduler.candidate list;
}

type finalization_state = {
  mutable actions: (unit -> unit) list;
  mutable artifacts: string list;
  initialized_logs: (string, unit) Hashtbl.t;
  mutable artifacts_cleaned: bool;
  mutable logs_finalized: bool;
}

type t = {
  freshness_mode: freshness_mode;
  session: Build_session.t;
  mutable cleaned: int;
  mutable previous_asts: int;
  mutable parsed: int;
  mutable compiled: int;
  mutable parse_seconds: float;
  mutable parse_messages: parse_message list;
  mutable diagnostics: string list;
  removed_modules: (string, unit) Hashtbl.t;
  preliminary_parses: (string, preliminary_parse) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  namespace_freshness: (string, float option) Hashtbl.t;
  package_removed_modules: (string, string list) Hashtbl.t;
  pending_work: pending_work;
  finalization: finalization_state;
  mutable compiler_cleaned: bool;
  mutable had_warnings: bool;
  process_poll: (unit -> unit) option;
  progress: Output.Progress.t;
  verbosity: int;
}

let create ~freshness_mode ~session ~process_poll ~progress ~verbosity =
  let removed_modules = Hashtbl.create 16 in
  Build_session.pending_removed_modules session
  |> List.iter (fun name -> Hashtbl.replace removed_modules name ());
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
    removed_modules;
    preliminary_parses = Hashtbl.create 16;
    blocked_modules = Hashtbl.create 16;
    namespace_freshness = Hashtbl.create 16;
    package_removed_modules = Hashtbl.create 32;
    pending_work = {namespace_jobs = []; compile_candidates = []};
    finalization =
      {
        actions = [];
        artifacts = [];
        initialized_logs = Hashtbl.create 16;
        artifacts_cleaned = false;
        logs_finalized = false;
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

let set_cleanup_result attempt root (result : Build_artifacts.cleanup_result) =
  Hashtbl.replace attempt.package_removed_modules root result.removed_modules;
  Build_session.set_public_outputs attempt.session root
    result.present_public_outputs

let removed_package_modules attempt root =
  Hashtbl.find_opt attempt.package_removed_modules root
  |> Option.value ~default:[]

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

let run_all actions =
  let first_error = ref None in
  List.iter
    (fun action ->
      try action ()
      with error ->
        if Option.is_none !first_error then first_error := Some error)
    actions;
  Option.iter raise !first_error

let cleanup_artifacts attempt =
  if not attempt.finalization.artifacts_cleaned then (
    attempt.finalization.artifacts_cleaned <- true;
    run_all
      (attempt.finalization.actions
      @ List.map
          (fun path () -> File_util.remove_file path)
          attempt.finalization.artifacts))

let finalize_logs attempt =
  if not attempt.finalization.logs_finalized then (
    attempt.finalization.logs_finalized <- true;
    let package_roots =
      attempt.finalization.initialized_logs |> Hashtbl.to_seq_keys
      |> List.of_seq
    in
    run_all
      ((fun () -> Output.Progress.finish attempt.progress)
      :: List.map
           (fun package_root () -> Compiler_log.finalize package_root)
           package_roots))

let finish_attempt attempt =
  run_all
    [(fun () -> cleanup_artifacts attempt); (fun () -> finalize_logs attempt)]

let protect attempt action =
  match action () with
  | result ->
    finish_attempt attempt;
    result
  | exception error ->
    finish_attempt attempt;
    raise error
