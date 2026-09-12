exception Stop

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed
type rebuild_kind = Incremental | Full

type source_root = {
  directory: string;
  recursive: bool;
  filter: Source_filter.t option;
}

type watch_scope = {
  roots: string list;
  paths: Native_watcher.watch_path list;
  sources: source_root list;
  unresolved: string list;
}

type snapshot_entry = {path: string; modified: float; size: int; digest: string}

let snapshot_entry_equal first second =
  first.path = second.path
  && first.modified = second.modified
  && first.size = second.size
  && first.digest = second.digest

let snapshot_equal = List.equal snapshot_entry_equal

type fallback = {
  message: string;
  scope: watch_scope;
  snapshot: snapshot_entry list;
}

type dependency_watch =
  | Resolved_dependency of Package_resolution.dependency
  | Broken_dependency of string
  | Missing_dependency_directory of string
  | Missing_dependency_path of string
  | Unresolved_dependency of string

let control_file_names = ["rescript.json"; "bsconfig.json"]
let is_control_file_name name = List.mem name control_file_names

let is_directory path =
  try (Unix.stat path).Unix.st_kind = Unix.S_DIR
  with Sys_error _ | Unix.Unix_error _ -> false

let rec nearest_existing_ancestor path =
  if is_directory path then Some (Platform.canonicalize_path path)
  else
    let parent = Filename.dirname path in
    if parent = path then None else nearest_existing_ancestor parent

let watch_context ~root ~prod ~features ~filter =
  try
    let root_config = Config.load_root root in
    let resolution =
      Package_resolution.create
        ~diagnostic_mode:Package_resolution.Suppress_diagnostics root_config
    in
    let visited = Hashtbl.create 32 in
    let packages = Hashtbl.create 32 in
    let requested_features = Feature_requests.create () in
    let roots = ref [root] in
    let paths = ref [] in
    let sources = ref [] in
    let unresolved = ref [] in
    let add_path directory recursive =
      paths := Native_watcher.{directory; recursive} :: !paths
    in
    let rec nearest_existing_directory package_root directory =
      if Sys.file_exists directory then directory
      else
        let parent = Filename.dirname directory in
        if parent = directory || directory = package_root then package_root
        else nearest_existing_directory package_root parent
    in
    let watch_unresolved_dependency package_root name =
      Package_resolution.dependency_candidates resolution ~package_root name
      |> List.iter (fun candidate ->
          let existing = nearest_existing_directory root candidate in
          try
            let canonical_existing = Platform.canonicalize_path existing in
            if Project_context.path_is_within_canonical ~root canonical_existing
            then (
              unresolved := candidate :: !unresolved;
              (* A shallow ancestor watch is sufficient: each directory creation
                wakes reconciliation, which advances the watch toward the complete
                candidate without expanding all of node_modules. *)
              add_path canonical_existing false)
          with Sys_error _ | Unix.Unix_error _ -> ())
    in
    let rec visit ~is_local ~features (config : Config.t) =
      Feature_requests.add requested_features config.root features;
      if Hashtbl.mem visited config.root then ()
      else (
        Hashtbl.add visited config.root ();
        if is_local then (
          Hashtbl.add packages config.root (config, true);
          add_path config.root false);
        let dependencies = Package_traversal.requests ~prod ~is_local config in
        let dependency_watches =
          List.map
            (fun (request : Package_traversal.request) ->
              let dependency = request.declaration in
              match
                Package_resolution.dependency_path resolution
                  ~package_root:config.root dependency.name
              with
              | Some directory when Config.exists_in_root directory -> (
                try
                  Resolved_dependency
                    (Package_resolution.resolve resolution
                       ~package_root:config.root dependency)
                with
                | Project_context.Package_error _ | Project_context.Error _ ->
                  Broken_dependency directory)
              | None -> Unresolved_dependency dependency.name
              | Some directory
                when (not (Config.exists_in_root directory))
                     && is_directory directory ->
                Missing_dependency_directory directory
              | Some path -> Missing_dependency_path path)
            dependencies
        in
        List.iter
          (function
            | Resolved_dependency resolved when resolved.is_local ->
              if not (Hashtbl.mem visited resolved.directory) then (
                roots := resolved.directory :: !roots;
                add_path resolved.directory false);
              visit ~is_local:true ~features:resolved.declaration.features
                resolved.config
            | Resolved_dependency resolved ->
              visit ~is_local:false ~features:resolved.declaration.features
                resolved.config
            | Broken_dependency directory ->
              (* A broken dependency configuration must remain watched so fixing
             that file can recover the long-lived command. *)
              roots := directory :: !roots;
              add_path directory false;
              Hashtbl.replace visited directory ()
            | Missing_dependency_directory directory ->
              (* The parent watch is needed because removing or replacing the
             watched directory itself is not reported consistently by every
             filesystem backend. It also detects a newly installed candidate
             that should take priority over a lower resolution. *)
              roots := directory :: !roots;
              unresolved := directory :: !unresolved;
              add_path (Filename.dirname directory) false;
              add_path directory false
            | Missing_dependency_path path ->
              (* A non-directory candidate may be replaced with an install. Watch
             its parent and retain its type in the snapshot until that happens. *)
              unresolved := path :: !unresolved;
              add_path (Filename.dirname path) false
            | Unresolved_dependency name ->
              watch_unresolved_dependency config.root name)
          dependency_watches)
    in
    visit ~is_local:true ~features root_config;
    Hashtbl.iter
      (fun package_root ((config : Config.t), is_local) ->
        try
          let requested =
            Feature_requests.find requested_features package_root
            |> Option.map Feature_requests.to_option
            |> Option.value ~default:None
          in
          Source.active_sources config
            ~prod:(Package_graph.source_discovery_prod ~prod ~is_local)
            ~features:requested
          |> List.iter (fun source ->
              let directory = Filename.concat config.root source.Config.dir in
              let filter =
                if package_root = root_config.root then filter else None
              in
              sources :=
                {directory; recursive = source.recurse; filter} :: !sources;
              let existing = nearest_existing_directory config.root directory in
              add_path existing (existing = directory && source.recurse))
        with Source.Error _ -> ())
      packages;
    let deduplicated_paths = Hashtbl.create (List.length !paths) in
    List.iter
      (fun (path : Native_watcher.watch_path) ->
        let recursive =
          path.recursive
          || Option.value
               (Hashtbl.find_opt deduplicated_paths path.directory)
               ~default:false
        in
        Hashtbl.replace deduplicated_paths path.directory recursive)
      !paths;
    let paths =
      Hashtbl.to_seq deduplicated_paths
      |> List.of_seq
      |> List.map (fun (directory, recursive) ->
          Native_watcher.{directory; recursive})
    in
    {
      roots = List.sort_uniq String.compare !roots;
      paths;
      sources = !sources;
      unresolved = List.sort_uniq String.compare !unresolved;
    }
  with Config.Error _ ->
    {
      roots = [root];
      paths = [Native_watcher.{directory = root; recursive = false}];
      sources = [];
      unresolved = [];
    }

let snapshot ?(on_source_symlink = fun _ -> ()) digest_cache scope =
  let visited_directories = Hashtbl.create 64 in
  let seen_files = Hashtbl.create 256 in
  let digest path stat =
    Hashtbl.replace seen_files path ();
    (* Native events are only wakeups; a content snapshot decides whether to
       rebuild. Reuse hashes while all cheap identity fields are unchanged. *)
    match Hashtbl.find_opt digest_cache path with
    | Some (mtime, ctime, size, digest)
      when mtime = stat.Unix.st_mtime && ctime = stat.Unix.st_ctime
           && size = stat.Unix.st_size ->
      digest
    | _ ->
      let digest = Digest.file path |> Digest.to_hex in
      Hashtbl.replace digest_cache path
        (stat.Unix.st_mtime, stat.Unix.st_ctime, stat.Unix.st_size, digest);
      digest
  in
  let add_file path stat acc =
    if Hashtbl.mem seen_files path then acc
    else
      let digest = digest path stat in
      {path; modified = stat.Unix.st_mtime; size = stat.Unix.st_size; digest}
      :: acc
  in
  let matches_source source path =
    Option.fold ~none:true
      ~some:(fun filter -> Source_filter.matches_basename filter path)
      source.filter
  in
  let rec walk source recursive dir acc =
    try
      let canonical = Platform.canonicalize_path dir in
      let previous = Hashtbl.find_opt visited_directories canonical in
      if previous = Some true || (previous = Some false && not recursive) then
        acc
      else (
        Hashtbl.replace visited_directories canonical recursive;
        let entries = Sys.readdir dir |> Array.to_list in
        List.fold_left
          (fun acc name ->
            let path = Filename.concat dir name in
            try
              let stat = Unix.lstat path in
              match stat.Unix.st_kind with
              | Unix.S_DIR ->
                if
                  (not recursive)
                  || Native_watcher.is_compiler_artifact_directory path
                then acc
                else walk source true path acc
              | Unix.S_LNK -> (
                let is_source_name =
                  (Filename.extension path = ".res"
                  || Filename.extension path = ".resi")
                  && matches_source source path
                in
                (if is_source_name then
                   try
                     let target = Unix.readlink path in
                     let target =
                       if Filename.is_relative target then
                         Filename.concat (Filename.dirname path) target
                       else target
                     in
                     let target =
                       try Platform.canonicalize_path target
                       with Sys_error _ | Unix.Unix_error _ -> target
                     in
                     on_source_symlink target
                   with Sys_error _ | Unix.Unix_error _ -> ());
                let target = Unix.stat path in
                match target.Unix.st_kind with
                | Unix.S_DIR when recursive -> walk source true path acc
                | Unix.S_REG when is_source_name -> add_file path target acc
                | _ -> acc)
              | Unix.S_REG
                when (Filename.extension path = ".res"
                     || Filename.extension path = ".resi")
                     && matches_source source path ->
                add_file path stat acc
              | _ -> acc
            with Sys_error _ | Unix.Unix_error _ -> acc)
          acc entries)
    with Sys_error _ | Unix.Unix_error _ -> acc
  in
  let add_control_files acc root =
    control_file_names
    |> List.fold_left
         (fun acc name ->
           let path = Filename.concat root name in
           try
             let stat = Unix.stat path in
             if stat.Unix.st_kind = Unix.S_REG then add_file path stat acc
             else acc
           with Sys_error _ | Unix.Unix_error _ -> acc)
         acc
  in
  let result =
    List.fold_left add_control_files [] scope.roots |> fun acc ->
    List.fold_left
      (fun acc source -> walk source source.recursive source.directory acc)
      acc scope.sources
    |> fun acc ->
    List.fold_left
      (fun acc path ->
        try
          let stat = Unix.lstat path in
          Hashtbl.replace seen_files path ();
          {
            path;
            modified = stat.Unix.st_mtime;
            size = stat.Unix.st_size;
            digest = "dependency-candidate";
          }
          :: acc
        with Sys_error _ | Unix.Unix_error _ ->
          {
            path;
            modified = 0.;
            size = 0;
            digest = "missing-dependency-candidate";
          }
          :: acc)
      acc scope.unresolved
    |> List.sort compare
  in
  Hashtbl.filter_map_inplace
    (fun path value -> if Hashtbl.mem seen_files path then Some value else None)
    digest_cache;
  result

let snapshot_with_symlink_paths digest_cache scope =
  let targets = ref [] in
  let snapshot =
    snapshot
      ~on_source_symlink:(fun target -> targets := target :: !targets)
      digest_cache scope
  in
  let paths =
    !targets
    |> List.sort_uniq String.compare
    |> List.filter_map (fun target ->
        Filename.dirname target |> nearest_existing_ancestor)
    |> List.concat_map (fun directory ->
        let parent = Filename.dirname directory in
        let directories =
          if parent = directory then [directory] else [directory; parent]
        in
        List.map
          (fun directory -> Native_watcher.{directory; recursive = false})
          directories)
  in
  (snapshot, paths, List.sort_uniq String.compare !targets)

let changes_between before after =
  (* A content snapshot deliberately treats native events only as wakeups. The
     before/after membership is still enough to distinguish an in-place edit,
     which can reuse the build graph, from a structural change that requires
     rediscovery. *)
  let before_by_path = Hashtbl.create (List.length before) in
  let after_by_path = Hashtbl.create (List.length after) in
  List.iter
    (fun entry -> Hashtbl.replace before_by_path entry.path entry)
    before;
  List.iter (fun entry -> Hashtbl.replace after_by_path entry.path entry) after;
  let changes = ref [] in
  Hashtbl.iter
    (fun path before_entry ->
      match Hashtbl.find_opt after_by_path path with
      | None -> changes := {path; kind = Removed} :: !changes
      | Some after_entry
        when not (snapshot_entry_equal before_entry after_entry) ->
        changes := {path; kind = Modified} :: !changes
      | Some _ -> ())
    before_by_path;
  Hashtbl.iter
    (fun path _ ->
      if not (Hashtbl.mem before_by_path path) then
        changes := {path; kind = Added} :: !changes)
    after_by_path;
  List.sort
    (fun (first : change) (second : change) ->
      String.compare first.path second.path)
    !changes

let update_snapshot_entries digest_cache previous changes =
  let entries = Hashtbl.create (List.length previous) in
  List.iter (fun entry -> Hashtbl.replace entries entry.path entry) previous;
  List.iter
    (fun change ->
      match change.kind with
      | Removed ->
        Hashtbl.remove entries change.path;
        Hashtbl.remove digest_cache change.path
      | Added | Modified -> (
        try
          let stat = Unix.stat change.path in
          let digest = Digest.file change.path |> Digest.to_hex in
          Hashtbl.replace digest_cache change.path
            (stat.Unix.st_mtime, stat.Unix.st_ctime, stat.Unix.st_size, digest);
          Hashtbl.replace entries change.path
            {
              path = change.path;
              modified = stat.Unix.st_mtime;
              size = stat.Unix.st_size;
              digest;
            }
        with Sys_error _ | Unix.Unix_error _ ->
          Hashtbl.remove entries change.path;
          Hashtbl.remove digest_cache change.path))
    changes;
  Hashtbl.to_seq_values entries |> List.of_seq |> List.sort compare

let polling_build_changes ~previous ~trigger ~before_build =
  if snapshot_equal trigger previous then []
  else changes_between previous before_build

let changes_are_incremental changes =
  changes <> []
  && List.for_all
       (fun change ->
         change.kind = Modified
         &&
         let extension = Filename.extension change.path in
         extension = ".res" || extension = ".resi")
       changes

let path_in_scope scope path =
  let name = Filename.basename path in
  let is_control =
    is_control_file_name name && List.mem (Filename.dirname path) scope.roots
  in
  let is_source =
    (Filename.extension path = ".res" || Filename.extension path = ".resi")
    && List.exists
         (fun source ->
           let in_directory =
             Filename.dirname path = source.directory
             || source.recursive
                && String.starts_with
                     ~prefix:(source.directory ^ Filename.dir_sep)
                     path
           in
           in_directory
           && Option.fold ~none:true
                ~some:(fun filter -> Source_filter.matches_basename filter path)
                source.filter)
         scope.sources
  in
  is_control || is_source || List.mem path scope.unresolved

let reconciliation_baseline ~old_scope ~new_scope before after =
  (* A configuration edit can add or remove files from the watch scope. Those
     membership changes are already covered by the build that read the new
     configuration; only changes to files shared by both scopes require
     another build. *)
  let shared path =
    path_in_scope old_scope path && path_in_scope new_scope path
  in
  List.filter (fun entry -> not (shared entry.path)) after
  @ List.filter (fun entry -> shared entry.path) before
  |> List.sort compare

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
  let refresh_and_snapshot watcher ~symlink_paths scope =
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
            snapshot_with_symlink_paths digest_cache scope
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
  let direct_content_changes watcher scope symlink_targets events =
    let changes = ref [] in
    let requires_reconciliation = ref false in
    let is_source_path path =
      let extension = Filename.extension path in
      extension = ".res" || extension = ".resi"
    in
    let is_in_source_tree path =
      List.exists
        (fun source ->
          path = source.directory
          || source.recursive
             && String.starts_with
                  ~prefix:(source.directory ^ Filename.dir_sep)
                  path)
        scope.sources
    in
    let is_directory path =
      try Sys.is_directory path with Sys_error _ -> false
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
            || path_in_scope scope path
            || Native_watcher.watches_directory watcher path
            || (is_in_source_tree path && is_directory path)
            || Native_watcher.watches_directory watcher (Filename.dirname path)
               && not (Build_artifacts.is_generated_output_path path)
          then requires_reconciliation := true
        | Native_watcher.Content, Some path ->
          if is_symlink_target path then requires_reconciliation := true
          else if is_source_path path then
            if path_in_scope scope path then
              if Sys.file_exists path then
                changes := {path; kind = Modified} :: !changes
              else requires_reconciliation := true
            else requires_reconciliation := true
          else if path_in_scope scope path then requires_reconciliation := true)
      events;
    if !requires_reconciliation then None
    else
      Some
        (!changes
        |> List.sort_uniq (fun (first : change) second ->
            String.compare first.path second.path))
  in
  let rec polling_loop scope previous =
    if keep_running () then
      let current = snapshot digest_cache scope in
      if not (snapshot_equal current previous) then (
        let build_scope = watch_context ~root ~prod ~features ~filter in
        let before_build = snapshot digest_cache build_scope in
        let changes =
          polling_build_changes ~previous ~trigger:current ~before_build
        in
        let rebuild_kind =
          if changes_are_incremental changes then Incremental else Full
        in
        Output.debug ~verbosity
          (match rebuild_kind with
          | Incremental -> "doing Incremental"
          | Full -> "doing Full");
        begin_rebuild rebuild_kind;
        build ~poll ~changes:(Some changes) |> finish_rebuild;
        let new_scope = watch_context ~root ~prod ~features ~filter in
        let after_build = snapshot digest_cache new_scope in
        let baseline =
          reconciliation_baseline ~old_scope:build_scope ~new_scope before_build
            after_build
        in
        delay 0.2;
        (* Keep the snapshot from before the rebuild when another edit lands
           during compilation. Otherwise that edit would become the new baseline
           and an atomic configuration rewrite could be missed. *)
        if not (snapshot_equal after_build baseline) then
          polling_loop new_scope baseline
        else polling_loop new_scope after_build)
      else (
        delay 0.2;
        polling_loop scope current)
  in
  let rec native_loop watcher scope symlink_targets previous =
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
      | Some changes ->
        Output.debug ~verbosity "doing Incremental";
        begin_rebuild Incremental;
        let before_build =
          update_snapshot_entries digest_cache previous changes
        in
        build ~poll ~changes:(Some changes) |> finish_rebuild;
        native_loop watcher scope symlink_targets before_build
      | None -> native_reconcile watcher scope previous)
  and native_reconcile watcher scope previous =
    let current = snapshot digest_cache scope in
    if not (snapshot_equal current previous) then (
      Output.debug ~verbosity "doing Full";
      begin_rebuild Full;
      let build_scope = watch_context ~root ~prod ~features ~filter in
      let before_build = snapshot digest_cache build_scope in
      build ~poll ~changes:(Some (changes_between previous current))
      |> finish_rebuild;
      let new_scope = watch_context ~root ~prod ~features ~filter in
      let after_build, symlink_paths, _ =
        snapshot_with_symlink_paths digest_cache new_scope
      in
      finish_reconciliation watcher ~old_scope:build_scope ~new_scope
        ~before:before_build ~after:after_build ~symlink_paths)
    else
      let new_scope = watch_context ~root ~prod ~features ~filter in
      let after_refresh, symlink_paths, _ =
        snapshot_with_symlink_paths digest_cache new_scope
      in
      finish_reconciliation watcher ~old_scope:scope ~new_scope ~before:current
        ~after:after_refresh ~symlink_paths
  and finish_reconciliation watcher ~old_scope ~new_scope ~before ~after
      ~symlink_paths =
    let baseline = reconciliation_baseline ~old_scope ~new_scope before after in
    match refresh_and_snapshot watcher ~symlink_paths new_scope with
    | Error message -> Some {message; scope = new_scope; snapshot = baseline}
    | Ok (registered_snapshot, symlink_targets) ->
      if not (snapshot_equal registered_snapshot baseline) then
        native_reconcile watcher new_scope baseline
      else native_loop watcher new_scope symlink_targets registered_snapshot
  in
  with_signal_handlers
    (fun _ -> stop ())
    (fun () ->
      let scope = watch_context ~root ~prod ~features ~filter in
      let before_build, symlink_paths, _ =
        snapshot_with_symlink_paths digest_cache scope
      in
      (* Install handles before the initial build so an edit made as soon as its
       output appears cannot land in a blind interval between compilation and
       watcher setup. Snapshot reconciliation below consumes any event queued
       while compiler subprocesses were running. *)
      match native_create ~paths:(scope.paths @ symlink_paths) with
      | Error message ->
        report_native_fallback message;
        ignore (build ~poll ~changes:None);
        let scope = watch_context ~root ~prod ~features ~filter in
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
  let is_control_file_name = is_control_file_name
  let polling_build_changes = polling_build_changes
  let changes_are_incremental = changes_are_incremental

  let run_with_native_failure ~message ~on_fallback =
    run_with_native_create
      ~native_create:(fun ~paths:_ -> Error message)
      ~report_native_fallback:on_fallback
end
