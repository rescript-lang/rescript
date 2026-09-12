exception Stop

type change_kind = Watch_snapshot.change_kind = Added | Removed | Modified
type change = Watch_snapshot.change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed
type rebuild_kind = Incremental | Full

type fallback = {
  message: string;
  scope: Watch_scope.t;
  snapshot: Watch_snapshot.entry list;
}

let with_signal_handlers handler f =
  let previous_sigint = Sys.signal Sys.sigint (Sys.Signal_handle handler) in
  Fun.protect
    (fun () ->
      let previous_sigterm =
        Sys.signal Sys.sigterm (Sys.Signal_handle handler)
      in
      Fun.protect f ~finally:(fun () ->
          ignore (Sys.signal Sys.sigterm previous_sigterm)))
    ~finally:(fun () -> ignore (Sys.signal Sys.sigint previous_sigint))

let run_locked ~native_create ~report_native_fallback ~root ~prod ~features
    ~filter ~clear_screen ~show_progress ~verbosity ~build ~watch_lock =
  let stop_requested = Atomic.make false in
  let stop () =
    (* Signal handlers only request termination because libuv may invoke them
       while a callback is being drained or a watch handle is being refreshed.
       Raising through that callback would be treated as an uncaught libuv
       exception and could bypass lock and handle cleanup. The build poll and
       watch-loop timer observe this flag, so shutdown remains prompt. *)
    Atomic.set stop_requested true
  in
  let digest_cache = Hashtbl.create 256 in
  let refresh_and_snapshot watcher ~symlink_paths (scope : Watch_scope.t) =
    (* Watch paths can change while handles are being installed, especially
       when a source symlink is repointed. A snapshot is authoritative only
       after its complete path set was registered. Continuous churn falls back
       to polling instead of accepting handles for an obsolete target. *)
    let rec loop remaining symlink_paths =
      if remaining = 0 then Error "watch paths did not stabilize"
      else
        match
          Native_watcher.refresh watcher ~paths:(scope.paths @ symlink_paths)
        with
        | Error _ as error -> error
        | Ok () ->
          let after_refresh, updated_symlink_paths, symlink_targets =
            Watch_snapshot.create_with_symlink_paths digest_cache scope
          in
          if updated_symlink_paths = symlink_paths then
            Ok (after_refresh, symlink_targets)
          else loop (remaining - 1) updated_symlink_paths
    in
    loop 8 symlink_paths
  in
  let keep_running () =
    (not (Atomic.get stop_requested)) && Build_lock.is_owned watch_lock
  in
  let next_lock_check = ref 0. in
  (* Lock removal is the test suite's portable shutdown protocol and must also
     interrupt an initial build, before native watch handles exist. Throttle
     ownership checks so the scheduler's frequent responsiveness ticks do not
     turn one source edit into a stream of identical filesystem reads. *)
  let poll () =
    if Atomic.get stop_requested then raise Stop;
    let now = Unix.gettimeofday () in
    if now >= !next_lock_check then (
      next_lock_check := now +. 0.1;
      if not (Build_lock.is_owned watch_lock) then raise Stop)
  in
  let delay seconds =
    (try ignore (Unix.select [] [] [] seconds)
     with Unix.Unix_error (Unix.EINTR, _, _) -> ());
    poll ()
  in
  let show_rebuild_presentation () =
    Output.should_clear_screen ~clear_screen ~show_progress
      ~interactive:(Unix.isatty Unix.stdout && Unix.isatty Unix.stderr)
  in
  let begin_rebuild kind =
    if show_rebuild_presentation () then (
      Printf.printf "\027[2J\027[H%!";
      print_endline
        (match kind with
        | Incremental -> "Change detected. Rebuilding..."
        | Full -> "Change detected. Full rebuild..."))
  in
  let finish_rebuild = function
    | Succeeded -> ()
    | Failed when show_rebuild_presentation () ->
      print_endline "\nBuild failed. Watching for changes..."
    | Failed -> ()
  in
  let direct_content_changes watcher (scope : Watch_scope.t) symlink_targets
      events =
    let changes = ref [] in
    let requires_reconciliation = ref false in
    let is_source_path path = Option.is_some (Source.source_kind path) in
    let is_in_source_tree path =
      List.exists
        (fun (source : Watch_scope.source_root) ->
          path = source.directory
          || source.recursive
             && String.starts_with
                  ~prefix:(source.directory ^ Filename.dir_sep)
                  path)
        scope.sources
    in
    let is_symlink_target path =
      let comparable = Platform.normalize_path_for_comparison path in
      List.exists
        (fun target ->
          Platform.normalize_path_for_comparison target = comparable)
        symlink_targets
    in
    List.iter
      (fun (event : Native_watcher.change) ->
        match (event.kind, event.path) with
        | _, None -> requires_reconciliation := true
        | Native_watcher.Structural, Some path ->
          if
            is_symlink_target path || is_source_path path
            || Watch_scope.path_in_scope scope path
            || Native_watcher.watches_directory watcher path
            || (is_in_source_tree path && File_util.is_directory path)
            || Native_watcher.watches_directory watcher (Filename.dirname path)
               && not (Build_artifacts.is_generated_output_path path)
          then requires_reconciliation := true
        | Native_watcher.Content, Some path ->
          if is_symlink_target path then requires_reconciliation := true
          else if is_source_path path then
            if Watch_scope.path_in_scope scope path then
              if File_util.exists path then
                changes := {path; kind = Modified} :: !changes
              else requires_reconciliation := true
            else requires_reconciliation := true
          else if Watch_scope.path_in_scope scope path then
            requires_reconciliation := true)
      events;
    if !requires_reconciliation then None
    else
      Some
        (!changes
        |> List.sort_uniq (fun (first : change) second ->
            String.compare first.path second.path))
  in
  let rec polling_loop (scope : Watch_scope.t) previous =
    if keep_running () then
      let current = Watch_snapshot.create digest_cache scope in
      if not (Watch_snapshot.equal current previous) then (
        let build_scope = Watch_scope.discover ~root ~prod ~features ~filter in
        let before_build = Watch_snapshot.create digest_cache build_scope in
        let changes =
          Watch_snapshot.polling_build_changes ~previous ~trigger:current
            ~before_build
        in
        let rebuild_kind =
          if Watch_snapshot.changes_are_incremental changes then Incremental
          else Full
        in
        Output.debug ~verbosity
          (match rebuild_kind with
          | Incremental -> "doing Incremental"
          | Full -> "doing Full");
        begin_rebuild rebuild_kind;
        build ~poll ~changes:(Some changes) |> finish_rebuild;
        let new_scope = Watch_scope.discover ~root ~prod ~features ~filter in
        let after_build = Watch_snapshot.create digest_cache new_scope in
        let baseline =
          Watch_snapshot.reconciliation_baseline ~old_scope:build_scope
            ~new_scope before_build after_build
        in
        delay 0.2;
        (* Keep the snapshot from before the rebuild when another edit lands
           during compilation. Otherwise that edit would become the new baseline
           and an atomic configuration rewrite could be missed. *)
        if not (Watch_snapshot.equal after_build baseline) then
          polling_loop new_scope baseline
        else polling_loop new_scope after_build)
      else (
        delay 0.2;
        polling_loop scope current)
  in
  let rec native_loop watcher (scope : Watch_scope.t) symlink_targets previous =
    let result = Native_watcher.wait watcher ~keep_running in
    match result with
    | Native_watcher.Stopped -> None
    | Native_watcher.Failed message ->
      Some {message; scope; snapshot = previous}
    | Native_watcher.Changed events -> (
      delay 0.05;
      let events = events @ Native_watcher.drain watcher in
      let direct =
        direct_content_changes watcher scope symlink_targets events
      in
      match direct with
      | Some [] -> native_loop watcher scope symlink_targets previous
      | Some changes -> (
        match Watch_snapshot.update_entries digest_cache previous changes with
        | Some before_build ->
          Output.debug ~verbosity "doing Incremental";
          begin_rebuild Incremental;
          build ~poll ~changes:(Some changes) |> finish_rebuild;
          native_loop watcher scope symlink_targets before_build
        | None -> native_reconcile watcher scope previous)
      | None -> native_reconcile watcher scope previous)
  and native_reconcile watcher (scope : Watch_scope.t) previous =
    let current = Watch_snapshot.create digest_cache scope in
    if not (Watch_snapshot.equal current previous) then (
      Output.debug ~verbosity "doing Full";
      begin_rebuild Full;
      let build_scope = Watch_scope.discover ~root ~prod ~features ~filter in
      let before_build = Watch_snapshot.create digest_cache build_scope in
      build ~poll
        ~changes:(Some (Watch_snapshot.changes_between previous current))
      |> finish_rebuild;
      let new_scope = Watch_scope.discover ~root ~prod ~features ~filter in
      let after_build, symlink_paths, _ =
        Watch_snapshot.create_with_symlink_paths digest_cache new_scope
      in
      finish_reconciliation watcher ~old_scope:build_scope ~new_scope
        ~before:before_build ~after:after_build ~symlink_paths)
    else
      let new_scope = Watch_scope.discover ~root ~prod ~features ~filter in
      let after_refresh, symlink_paths, _ =
        Watch_snapshot.create_with_symlink_paths digest_cache new_scope
      in
      finish_reconciliation watcher ~old_scope:scope ~new_scope ~before:current
        ~after:after_refresh ~symlink_paths
  and finish_reconciliation watcher ~(old_scope : Watch_scope.t)
      ~(new_scope : Watch_scope.t) ~before ~after ~symlink_paths =
    let baseline =
      Watch_snapshot.reconciliation_baseline ~old_scope ~new_scope before after
    in
    match refresh_and_snapshot watcher ~symlink_paths new_scope with
    | Error message -> Some {message; scope = new_scope; snapshot = baseline}
    | Ok (registered_snapshot, symlink_targets) ->
      if not (Watch_snapshot.equal registered_snapshot baseline) then
        native_reconcile watcher new_scope baseline
      else native_loop watcher new_scope symlink_targets registered_snapshot
  in
  with_signal_handlers
    (fun _ -> stop ())
    (fun () ->
      let scope = Watch_scope.discover ~root ~prod ~features ~filter in
      let before_build, symlink_paths, _ =
        Watch_snapshot.create_with_symlink_paths digest_cache scope
      in
      (* Install handles before the initial build so an edit made as soon as its
       output appears cannot land in a blind interval between compilation and
       watcher setup. Snapshot reconciliation below consumes any event queued
       while compiler subprocesses were running. *)
      match native_create ~paths:(scope.paths @ symlink_paths) with
      | Error message ->
        report_native_fallback message;
        ignore (build ~poll ~changes:None);
        let scope = Watch_scope.discover ~root ~prod ~features ~filter in
        polling_loop scope before_build
      | Ok watcher ->
        let fallback =
          Fun.protect
            (fun () ->
              ignore (build ~poll ~changes:None);
              (* The following snapshot is authoritative for changes that arrived
               during the build. Pump and discard callbacks already queued for
               that interval so they do not request the same rebuild twice. *)
              ignore (Native_watcher.drain watcher);
              native_reconcile watcher scope before_build)
            ~finally:(fun () -> Native_watcher.close watcher)
        in
        Option.iter
          (fun fallback ->
            report_native_fallback fallback.message;
            polling_loop fallback.scope fallback.snapshot)
          fallback)

let run_with_native_create ~native_create ~report_native_fallback ~root ~prod
    ~features ~filter ~clear_screen ~show_progress ~verbosity ~build =
  Build_lock.with_watch root (fun watch_lock ->
      ignore (Config.load_root root);
      run_locked ~native_create ~report_native_fallback ~root ~prod ~features
        ~filter ~clear_screen ~show_progress ~verbosity ~build ~watch_lock)

let report_native_fallback message =
  prerr_endline
    ("Native file watching is unavailable (" ^ message
   ^ "); falling back to polling")

let run =
  run_with_native_create ~native_create:Native_watcher.create
    ~report_native_fallback

module For_test = struct
  let is_control_file_name = Watch_scope.is_control_file_name

  let run_with_native_failure ~message ~on_fallback =
    run_with_native_create
      ~native_create:(fun ~paths:_ -> Error message)
      ~report_native_fallback:on_fallback
end
