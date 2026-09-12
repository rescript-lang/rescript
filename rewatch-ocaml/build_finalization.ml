type t = {
  attempt: Build_attempt.t;
  progress: Output.Progress.t;
  mutable artifacts_cleaned: bool;
  mutable logs_finalized: bool;
}

let run_all actions =
  let first_error = ref None in
  List.iter
    (fun action ->
      try action ()
      with error ->
        if Option.is_none !first_error then first_error := Some error)
    actions;
  Option.iter raise !first_error

let create ~attempt ~progress =
  {attempt; progress; artifacts_cleaned = false; logs_finalized = false}

let cleanup_artifacts finalization =
  if not finalization.artifacts_cleaned then (
    finalization.artifacts_cleaned <- true;
    let cleanup = Build_attempt.take_cleanup finalization.attempt in
    run_all
      (cleanup.actions
      @ List.map (fun path () -> File_util.remove_file path) cleanup.artifacts))

let finalize_logs finalization =
  if not finalization.logs_finalized then (
    finalization.logs_finalized <- true;
    let package_roots =
      Build_attempt.take_initialized_logs finalization.attempt
    in
    run_all
      ((fun () -> Output.Progress.finish finalization.progress)
      :: List.map
           (fun package_root () -> Compiler_log.finalize package_root)
           package_roots))

let finish finalization =
  run_all
    [
      (fun () -> cleanup_artifacts finalization);
      (fun () -> finalize_logs finalization);
    ]
