exception Stop

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed
type rebuild_kind = Incremental | Full

type source_root = {directory: string; recursive: bool}

let is_directory path =
  try (Unix.stat path).Unix.st_kind = Unix.S_DIR
  with Sys_error _ | Unix.Unix_error _ -> false

let rec nearest_existing_ancestor path =
  if is_directory path then Some (Unix.realpath path)
  else
    let parent = Filename.dirname path in
    if parent = path then None else nearest_existing_ancestor parent

let watch_context ~root ~prod ~features =
  try
    let root_config = Config.load_root root in
    let dependency_context = Project_context.dependency_context root_config in
  let visited = Hashtbl.create 32 in
  let packages = Hashtbl.create 32 in
  let requested_features = Hashtbl.create 32 in
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
  let add_feature_request root request =
    match Hashtbl.find_opt requested_features root, request with
    | None, request -> Hashtbl.add requested_features root request
    | Some None, _ | Some _, None -> Hashtbl.replace requested_features root None
    | Some (Some current), Some requested ->
      Hashtbl.replace requested_features root
        (Some (List.sort_uniq String.compare (current @ requested)))
  in
  let path_is_within_root path =
    let normalize = Platform.normalize_path_for_comparison in
    let root = normalize root in
    let path = normalize path in
    path = root || String.starts_with ~prefix:(Filename.concat root "") path
  in
  let watch_unresolved_dependency package_root name =
    Project_context.dependency_candidates_in dependency_context package_root name
    |> List.iter (fun candidate ->
         let existing = nearest_existing_directory root candidate in
         try
           let canonical_existing = Unix.realpath existing in
           if path_is_within_root canonical_existing then (
             unresolved := candidate :: !unresolved;
             (* A shallow ancestor watch is sufficient: each directory creation
                wakes reconciliation, which advances the watch toward the complete
                candidate without expanding all of node_modules. *)
             add_path canonical_existing false)
         with Sys_error _ | Unix.Unix_error _ -> ())
  in
  let rec visit ~is_local ~features (config : Config.t) =
    add_feature_request config.root features;
    if Hashtbl.mem visited config.root then ()
    else (
    Hashtbl.add visited config.root ();
    Hashtbl.add packages config.root (config, is_local);
    add_path config.root false;
    let dependencies =
      config.dependencies
      @ if prod || not is_local then [] else config.dev_dependencies
    in
    List.iter
      (fun (dependency : Config.dependency) ->
        match
          Project_context.dependency_path_in dependency_context config.root
            dependency.name
        with
        | Some directory
          when Project_context.dependency_is_local_canonical dependency_context
                 directory
               && Config.exists_in_root directory ->
          if not (Hashtbl.mem visited directory) then (
            (* A broken dependency configuration must remain watched so fixing
               that file can recover the long-lived command. *)
            roots := directory :: !roots;
            add_path directory false);
          (try
             visit
               ~is_local:
                 (Project_context.dependency_is_local_canonical
                    dependency_context directory)
               ~features:dependency.features (Config.load_root directory)
           with Config.Error _ -> Hashtbl.replace visited directory ())
        | None -> watch_unresolved_dependency config.root dependency.name
        | Some directory
          when not (Config.exists_in_root directory) && is_directory directory ->
          (* The parent watch is needed because removing or replacing the
             watched directory itself is not reported consistently by every
             filesystem backend. It also detects a newly installed candidate
             that should take priority over a lower resolution. *)
          roots := directory :: !roots;
          unresolved := directory :: !unresolved;
          add_path (Filename.dirname directory) false;
          add_path directory false
        | Some path when not (Config.exists_in_root path) ->
          (* A non-directory candidate may be replaced with an install. Watch
             its parent and retain its type in the snapshot until that happens. *)
          unresolved := path :: !unresolved;
          add_path (Filename.dirname path) false
        | Some _ -> ())
      dependencies)
  in
    visit ~is_local:true ~features root_config;
    Hashtbl.iter
      (fun package_root ((config : Config.t), is_local) ->
        try
          let requested = Hashtbl.find_opt requested_features package_root in
          let requested = Option.value requested ~default:None in
          let all_features = Option.is_none requested in
          let active_features =
            Source.resolve_active_features config
              (Option.value requested ~default:[])
          in
          config.sources
          |> List.filter (fun (source : Config.source) ->
               let feature_enabled =
                 all_features
                 || Option.fold ~none:true
                      ~some:(fun feature ->
                        Hashtbl.mem active_features feature)
                      source.feature
               in
               (not (Package_graph.source_discovery_prod ~prod ~is_local
                     && source.is_dev))
               && feature_enabled)
          |> List.iter (fun source ->
               let directory = Filename.concat config.root source.Config.dir in
               sources := {directory; recursive = source.recurse} :: !sources;
               let existing =
                 nearest_existing_directory config.root directory
               in
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
      Hashtbl.to_seq deduplicated_paths |> List.of_seq
      |> List.map (fun (directory, recursive) ->
           Native_watcher.{directory; recursive})
    in
    ( List.sort_uniq String.compare !roots,
      paths,
      !sources,
      List.sort_uniq String.compare !unresolved )
  with Config.Error _ ->
    ( [root],
      [Native_watcher.{directory = root; recursive = false}],
      [],
      [] )

let snapshot ?(on_source_symlink = fun _ -> ()) digest_cache ~matches_source
    roots sources unresolved =
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
      (path, stat.Unix.st_mtime, stat.Unix.st_size, digest) :: acc
  in
  let rec walk recursive dir acc =
    try
      let canonical = Unix.realpath dir in
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
                  || List.mem name ["lib"; "node_modules"; ".git"; "_build"]
                then
                  acc
                else walk true path acc
              | Unix.S_LNK -> (
                let is_source_name =
                  (Filename.extension path = ".res"
                  || Filename.extension path = ".resi")
                  && matches_source path
                in
                if is_source_name then (
                  try
                    let target = Unix.readlink path in
                    let target =
                      if Filename.is_relative target then
                        Filename.concat (Filename.dirname path) target
                      else target
                    in
                    Filename.dirname target |> nearest_existing_ancestor
                    |> Option.iter on_source_symlink
                  with Sys_error _ | Unix.Unix_error _ -> ());
                let target = Unix.stat path in
                match target.Unix.st_kind with
                | Unix.S_DIR when recursive -> walk true path acc
                | Unix.S_REG when is_source_name -> add_file path target acc
                | _ -> acc)
              | Unix.S_REG
                when (Filename.extension path = ".res"
                     || Filename.extension path = ".resi")
                     && matches_source path ->
                add_file path stat acc
              | _ -> acc
            with Sys_error _ | Unix.Unix_error _ -> acc)
          acc entries)
    with Sys_error _ | Unix.Unix_error _ -> acc
  in
  let add_control_files acc root =
    List.fold_left
      (fun acc name ->
        let path = Filename.concat root name in
        try
          let stat = Unix.stat path in
          if stat.Unix.st_kind = Unix.S_REG then add_file path stat acc else acc
        with Sys_error _ | Unix.Unix_error _ -> acc)
      acc ["rescript.json"; "bsconfig.json"; "package.json"]
  in
  let result =
    List.fold_left add_control_files [] roots
    |> fun acc ->
    List.fold_left
      (fun acc source -> walk source.recursive source.directory acc)
      acc sources
    |> fun acc ->
    List.fold_left
      (fun acc path ->
        try
          let stat = Unix.lstat path in
          Hashtbl.replace seen_files path ();
          ( path,
            stat.Unix.st_mtime,
            stat.Unix.st_size,
            "dependency-candidate" )
          :: acc
        with Sys_error _ | Unix.Unix_error _ ->
          (path, 0., 0, "missing-dependency-candidate") :: acc)
      acc unresolved
    |> List.sort compare
  in
  Hashtbl.filter_map_inplace
    (fun path value -> if Hashtbl.mem seen_files path then Some value else None)
  digest_cache;
  result

let snapshot_with_symlink_paths digest_cache ~matches_source roots sources
    unresolved =
  let target_parents = ref [] in
  let snapshot =
    snapshot
      ~on_source_symlink:(fun directory ->
        target_parents := directory :: !target_parents)
      digest_cache ~matches_source roots sources unresolved
  in
  let paths =
    !target_parents |> List.sort_uniq String.compare
    |> List.concat_map (fun directory ->
         let parent = Filename.dirname directory in
         let directories = if parent = directory then [directory] else [directory; parent] in
         List.map
           (fun directory -> Native_watcher.{directory; recursive = false})
           directories)
  in
  (snapshot, paths)

let changes_between before after =
  (* A content snapshot deliberately treats native events only as wakeups. The
     before/after membership is still enough to distinguish an in-place edit,
     which can reuse the build graph, from a structural change that requires
     rediscovery. *)
  let before_by_path = Hashtbl.create (List.length before) in
  let after_by_path = Hashtbl.create (List.length after) in
  List.iter
    (fun ((path, _, _, _) as entry) -> Hashtbl.replace before_by_path path entry)
    before;
  List.iter
    (fun ((path, _, _, _) as entry) -> Hashtbl.replace after_by_path path entry)
    after;
  let changes = ref [] in
  Hashtbl.iter
    (fun path before_entry ->
      match Hashtbl.find_opt after_by_path path with
      | None -> changes := {path; kind = Removed} :: !changes
      | Some after_entry when before_entry <> after_entry ->
        changes := {path; kind = Modified} :: !changes
      | Some _ -> ())
    before_by_path;
  Hashtbl.iter
    (fun path _ ->
      if not (Hashtbl.mem before_by_path path) then
        changes := {path; kind = Added} :: !changes)
    after_by_path;
  List.sort (fun first second -> String.compare first.path second.path) !changes

let update_snapshot_entries digest_cache previous changes =
  let entries = Hashtbl.create (List.length previous) in
  List.iter
    (fun ((path, _, _, _) as entry) -> Hashtbl.replace entries path entry)
    previous;
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
            (change.path, stat.Unix.st_mtime, stat.Unix.st_size, digest)
        with Sys_error _ | Unix.Unix_error _ ->
          Hashtbl.remove entries change.path;
          Hashtbl.remove digest_cache change.path))
    changes;
  Hashtbl.to_seq_values entries |> List.of_seq |> List.sort compare

let polling_build_changes ~previous ~trigger ~before_build =
  if trigger = previous then [] else changes_between previous before_build

let changes_are_incremental changes =
  changes <> []
  && List.for_all
       (fun change ->
         change.kind = Modified
         &&
         let extension = Filename.extension change.path in
         extension = ".res" || extension = ".resi")
       changes

let path_in_scope roots sources unresolved path =
  let name = Filename.basename path in
  let is_control =
    List.mem name ["rescript.json"; "bsconfig.json"; "package.json"]
    && List.mem (Filename.dirname path) roots
  in
  let is_source =
    (Filename.extension path = ".res" || Filename.extension path = ".resi")
    && List.exists
         (fun source ->
           Filename.dirname path = source.directory
           || (source.recursive
              && String.starts_with
                   ~prefix:(source.directory ^ Filename.dir_sep)
                   path))
         sources
  in
  is_control || is_source || List.mem path unresolved

let reconciliation_baseline ~old_roots ~old_sources ~old_unresolved ~new_roots
    ~new_sources ~new_unresolved before after =
  (* A configuration edit can add or remove files from the watch scope. Those
     membership changes are already covered by the build that read the new
     configuration; only changes to files shared by both scopes require
     another build. *)
  let shared path =
    path_in_scope old_roots old_sources old_unresolved path
    && path_in_scope new_roots new_sources new_unresolved path
  in
  List.filter (fun (path, _, _, _) -> not (shared path)) after
  @ List.filter (fun (path, _, _, _) -> shared path) before
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
  let stop_requested = ref false in
  let stop () =
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    Sys.set_signal Sys.sigterm Sys.Signal_ignore;
    (* Signal handlers only request termination because libuv may invoke them
       while a callback is being drained or a watch handle is being refreshed.
       Raising through that callback would be treated as an uncaught libuv
       exception and could bypass lock and handle cleanup. The build poll and
       watch-loop timer observe this flag, so shutdown remains prompt. *)
    stop_requested := true
  in
  let digest_cache = Hashtbl.create 256 in
  let matches_filter =
    match filter with
    | None -> fun _ -> true
    | Some pattern ->
      let regex = Str.regexp pattern in
      fun path ->
        try
          ignore (Str.search_forward regex (Filename.basename path) 0);
          true
        with Not_found -> false
  in
  let refresh_and_snapshot watcher ~paths ~symlink_paths roots sources
      unresolved =
    (* Watch paths can change while handles are being installed, especially
       when a source symlink is repointed. A snapshot is authoritative only
       after its complete path set was registered. Continuous churn falls back
       to polling instead of accepting handles for an obsolete target. *)
    let rec loop remaining symlink_paths =
      if remaining = 0 then Error "watch paths did not stabilize"
      else
        match Native_watcher.refresh watcher ~paths:(paths @ symlink_paths) with
        | Error _ as error -> error
        | Ok () ->
          let after_refresh, updated_symlink_paths =
            snapshot_with_symlink_paths digest_cache
              ~matches_source:matches_filter roots sources unresolved
          in
          if updated_symlink_paths = symlink_paths then Ok after_refresh
          else loop (remaining - 1) updated_symlink_paths
    in
    loop 8 symlink_paths
  in
  let keep_running () =
    (not !stop_requested) && Build_lock.is_owned watch_lock
  in
  let next_lock_check = ref 0. in
  (* Lock removal is the test suite's portable shutdown protocol and must also
     interrupt an initial build, before native watch handles exist. Throttle
     ownership checks so the scheduler's frequent responsiveness ticks do not
     turn one source edit into a stream of identical filesystem reads. *)
  let poll () =
    if !stop_requested then raise Stop;
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
  let direct_content_changes watcher roots sources unresolved events =
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
          || (source.recursive
             && String.starts_with
                  ~prefix:(source.directory ^ Filename.dir_sep)
                  path))
        sources
    in
    let is_directory path =
      try Sys.is_directory path with Sys_error _ -> false
    in
    List.iter
      (fun (event : Native_watcher.change) ->
        match event.kind, event.path with
        | _, None ->
          requires_reconciliation := true
        | Native_watcher.Structural, Some path ->
          if
            is_source_path path || path_in_scope roots sources unresolved path
            || Native_watcher.watches_directory watcher path
            || (is_in_source_tree path && is_directory path)
            || (Native_watcher.watches_directory watcher (Filename.dirname path)
               && not (Build_artifacts.is_generated_output_path path))
          then
            requires_reconciliation := true
        | Native_watcher.Content, Some path ->
          if is_source_path path then (
            if path_in_scope roots sources unresolved path then (
              if matches_filter path then
                if Sys.file_exists path then
                  changes := {path; kind = Modified} :: !changes
                else requires_reconciliation := true)
            else requires_reconciliation := true)
          else if path_in_scope roots sources unresolved path then
            requires_reconciliation := true)
      events;
    if !requires_reconciliation then None
    else
      Some
        (!changes
        |> List.sort_uniq (fun (first : change) second ->
             String.compare first.path second.path))
  in
  let rec polling_loop roots sources unresolved previous =
    if keep_running () then (
      let current =
        snapshot digest_cache ~matches_source:matches_filter roots sources
          unresolved
      in
      if current <> previous then (
        let build_roots, _, build_sources, build_unresolved =
          watch_context ~root ~prod ~features
        in
        let before_build =
          snapshot digest_cache ~matches_source:matches_filter build_roots
            build_sources build_unresolved
        in
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
        build ~poll ~changes:(Some changes)
        |> finish_rebuild;
        let new_roots, _, new_sources, new_unresolved =
          watch_context ~root ~prod ~features
        in
        let after_build =
          snapshot digest_cache ~matches_source:matches_filter new_roots
            new_sources new_unresolved
        in
        let baseline =
          reconciliation_baseline ~old_roots:build_roots
            ~old_sources:build_sources ~old_unresolved:build_unresolved
            ~new_roots ~new_sources ~new_unresolved before_build after_build
        in
        delay 0.2;
        (* Keep the snapshot from before the rebuild when another edit lands
           during compilation. Otherwise that edit would become the new baseline
           and an atomic configuration rewrite could be missed. *)
        if after_build <> baseline then
          polling_loop new_roots new_sources new_unresolved baseline
        else polling_loop new_roots new_sources new_unresolved after_build)
      else (
        delay 0.2;
        polling_loop roots sources unresolved current))
  in
  let rec native_loop watcher roots sources unresolved previous =
    let result = Native_watcher.wait watcher ~keep_running in
    match result with
    | Native_watcher.Stopped -> None
    | Native_watcher.Failed message ->
      Some (message, roots, sources, unresolved, previous)
    | Native_watcher.Changed events ->
      delay 0.05;
      let events = events @ Native_watcher.drain watcher in
      let direct =
        direct_content_changes watcher roots sources unresolved events
      in
      (match direct with
      | Some [] -> native_loop watcher roots sources unresolved previous
      | Some changes ->
        Output.debug ~verbosity "doing Incremental";
        begin_rebuild Incremental;
        build ~poll ~changes:(Some changes) |> finish_rebuild;
        let updated_previous =
          update_snapshot_entries digest_cache previous changes
        in
        native_loop watcher roots sources unresolved updated_previous
      | None -> native_reconcile watcher roots sources unresolved previous)
  and native_reconcile watcher roots sources unresolved previous =
    let current =
      snapshot digest_cache ~matches_source:matches_filter roots sources
        unresolved
    in
    if current <> previous then (
      Output.debug ~verbosity "doing Full";
      begin_rebuild Full;
      let build_roots, _, build_sources, build_unresolved =
        watch_context ~root ~prod ~features
      in
      let before_build =
        snapshot digest_cache ~matches_source:matches_filter build_roots
          build_sources build_unresolved
      in
      build ~poll ~changes:(Some (changes_between previous current))
      |> finish_rebuild;
      let new_roots, paths, new_sources, new_unresolved =
        watch_context ~root ~prod ~features
      in
      let after_build, symlink_paths =
        snapshot_with_symlink_paths digest_cache
          ~matches_source:matches_filter new_roots new_sources new_unresolved
      in
      let baseline =
        reconciliation_baseline ~old_roots:build_roots
          ~old_sources:build_sources ~old_unresolved:build_unresolved
          ~new_roots ~new_sources ~new_unresolved before_build after_build
      in
      match
        refresh_and_snapshot watcher ~paths ~symlink_paths new_roots new_sources
          new_unresolved
      with
      | Error message ->
        Some (message, new_roots, new_sources, new_unresolved, baseline)
      | Ok registered_snapshot ->
        if registered_snapshot <> baseline then
          native_reconcile watcher new_roots new_sources new_unresolved baseline
        else
          native_loop watcher new_roots new_sources new_unresolved
            registered_snapshot)
    else
      let new_roots, paths, new_sources, new_unresolved =
        watch_context ~root ~prod ~features
      in
      let after_refresh, symlink_paths =
        snapshot_with_symlink_paths digest_cache
          ~matches_source:matches_filter new_roots new_sources new_unresolved
      in
      let baseline =
        reconciliation_baseline ~old_roots:roots ~old_sources:sources
          ~old_unresolved:unresolved ~new_roots ~new_sources ~new_unresolved
          current after_refresh
      in
      match
        refresh_and_snapshot watcher ~paths ~symlink_paths new_roots new_sources
          new_unresolved
      with
      | Error message ->
        Some (message, new_roots, new_sources, new_unresolved, baseline)
      | Ok registered_snapshot ->
        if registered_snapshot <> baseline then
          native_reconcile watcher new_roots new_sources new_unresolved baseline
        else
          native_loop watcher new_roots new_sources new_unresolved
            registered_snapshot
  in
  with_signal_handlers (fun _ -> stop ()) (fun () ->
    let roots, paths, sources, unresolved =
      watch_context ~root ~prod ~features
    in
    let before_build, symlink_paths =
      snapshot_with_symlink_paths digest_cache
        ~matches_source:matches_filter roots sources unresolved
    in
    (* Install handles before the initial build so an edit made as soon as its
       output appears cannot land in a blind interval between compilation and
       watcher setup. Snapshot reconciliation below consumes any event queued
       while compiler subprocesses were running. *)
    match native_create ~paths:(paths @ symlink_paths) with
    | Error message ->
      report_native_fallback message;
      ignore (build ~poll ~changes:None);
      let roots, _, sources, unresolved =
        watch_context ~root ~prod ~features
      in
      polling_loop roots sources unresolved before_build
    | Ok watcher ->
      let fallback =
        Fun.protect
          (fun () ->
            ignore (build ~poll ~changes:None);
            (* The following snapshot is authoritative for changes that arrived
               during the build. Pump and discard callbacks already queued for
               that interval so they do not request the same rebuild twice. *)
            ignore (Native_watcher.drain watcher);
            native_reconcile watcher roots sources unresolved before_build)
          ~finally:(fun () -> Native_watcher.close watcher)
      in
      Option.iter
        (fun (message, roots, sources, unresolved, previous) ->
          report_native_fallback message;
          polling_loop roots sources unresolved previous)
        fallback)

let run_with_native_create ~native_create ~report_native_fallback ~root ~prod
    ~features ~filter ~clear_screen ~show_progress ~verbosity ~build =
  Build_lock.with_watch root (fun watch_lock ->
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
  let polling_build_changes = polling_build_changes
  let changes_are_incremental = changes_are_incremental

  let run_with_native_failure ~message ~on_fallback =
    run_with_native_create ~native_create:(fun ~paths:_ -> Error message)
      ~report_native_fallback:on_fallback
end
