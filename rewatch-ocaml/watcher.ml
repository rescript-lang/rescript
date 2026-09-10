exception Stop

let watch_context ~root ~prod =
  let visited = Hashtbl.create 32 in
  Hashtbl.add visited root ();
  let roots = ref [root] in
  let paths = ref [] in
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
  let rec visit (config : Config.t) =
    add_path config.root false;
    config.sources
    |> List.filter (fun source -> (not prod) || not source.Config.is_dev)
    |> List.iter (fun source ->
         let directory = Filename.concat config.root source.Config.dir in
         let existing = nearest_existing_directory config.root directory in
         add_path existing (existing = directory && source.Config.recurse));
    let dependencies =
      config.dependencies @ if prod then [] else config.dev_dependencies
    in
    List.iter
      (fun (dependency : Config.dependency) ->
        match Project_context.dependency_path config.root dependency.name with
        | Some directory
          when (not (Hashtbl.mem visited directory))
               && Project_context.is_local_dependency_canonical ~workspace:root
                    directory
               && Config.exists_in_root directory ->
          Hashtbl.add visited directory ();
          roots := directory :: !roots;
          visit (Config.load_root directory)
        | _ -> ())
      dependencies
  in
  try
    visit (Config.load_root root);
    (List.sort String.compare !roots, !paths)
  with Config.Error _ ->
    ([root], [Native_watcher.{directory = root; recursive = false}])

let snapshot digest_cache roots =
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
  let rec walk dir acc =
    try
      let canonical = Unix.realpath dir in
      if Hashtbl.mem visited_directories canonical then acc
      else (
        Hashtbl.add visited_directories canonical ();
        let entries = Sys.readdir dir |> Array.to_list in
        List.fold_left
          (fun acc name ->
            let path = Filename.concat dir name in
            try
              let stat = Unix.lstat path in
              match stat.Unix.st_kind with
              | Unix.S_DIR ->
                if List.mem name ["lib"; "node_modules"; ".git"; "_build"] then
                  acc
                else walk path acc
              | Unix.S_LNK ->
                if (Unix.stat path).Unix.st_kind = Unix.S_DIR then walk path acc
                else acc
              | Unix.S_REG
                when Filename.extension path = ".res"
                     || Filename.extension path = ".resi"
                     || name = "rescript.json" || name = "bsconfig.json"
                     || name = "package.json" ->
                let digest = digest path stat in
                (path, stat.Unix.st_mtime, stat.Unix.st_size, digest) :: acc
              | _ -> acc
            with Sys_error _ | Unix.Unix_error _ -> acc)
          acc entries)
    with Sys_error _ | Unix.Unix_error _ -> acc
  in
  let result =
    List.sort compare (List.concat_map (fun directory -> walk directory []) roots)
  in
  Hashtbl.filter_map_inplace
    (fun path value -> if Hashtbl.mem seen_files path then Some value else None)
    digest_cache;
  result

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

let run_locked ~root ~prod ~clear_screen ~show_progress ~build ~watch_lock =
  let stop_requested = ref false in
  let waiting_for_native_event = ref false in
  let stop () =
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    Sys.set_signal Sys.sigterm Sys.Signal_ignore;
    (* Do not raise through libuv while it owns the callback stack. Its
       keep-running predicate closes the handles and returns Stopped instead. *)
    if !waiting_for_native_event then stop_requested := true else raise Stop
  in
  let digest_cache = Hashtbl.create 256 in
  let keep_running () =
    (not !stop_requested) && Build_lock.is_owned watch_lock
  in
  let poll () = if not (keep_running ()) then raise Stop in
  let clear_terminal () =
    if
      Output.should_clear_screen ~clear_screen ~show_progress
        ~interactive:(Unix.isatty Unix.stdout && Unix.isatty Unix.stderr)
    then
      Printf.printf "\027[2J\027[H%!"
  in
  let rec polling_loop roots previous =
    if keep_running () then (
      let current = snapshot digest_cache roots in
      if current <> previous then (
        clear_terminal ();
        build ~poll;
        let roots, _ = watch_context ~root ~prod in
        let after_build = snapshot digest_cache roots in
        ignore (Unix.select [] [] [] 0.2);
        (* Keep the snapshot from before the rebuild when another edit lands
           during compilation. Otherwise that edit would become the new baseline
           and an atomic configuration rewrite could be missed. *)
        if after_build <> current then polling_loop roots current
        else polling_loop roots after_build)
      else (
        ignore (Unix.select [] [] [] 0.2);
        polling_loop roots current))
  in
  let native_fallback message =
    prerr_endline
      ("Native file watching is unavailable (" ^ message
     ^ "); falling back to polling")
  in
  let rec native_loop watcher roots previous =
    waiting_for_native_event := true;
    let result =
      Fun.protect
        (fun () -> Native_watcher.wait watcher ~keep_running)
        ~finally:(fun () -> waiting_for_native_event := false)
    in
    match result with
    | Native_watcher.Stopped -> None
    | Native_watcher.Failed message -> Some (message, roots, previous)
    | Native_watcher.Changed ->
      ignore (Unix.select [] [] [] 0.05);
      native_reconcile watcher roots previous
  and native_reconcile watcher roots previous =
    let current = snapshot digest_cache roots in
    if current <> previous then (
      clear_terminal ();
      build ~poll;
      let roots, paths = watch_context ~root ~prod in
      match Native_watcher.refresh watcher ~paths with
      | Error message -> Some (message, roots, current)
      | Ok () ->
        let after_build = snapshot digest_cache roots in
        if after_build <> current then native_reconcile watcher roots current
        else native_loop watcher roots after_build)
    else
      let _, paths = watch_context ~root ~prod in
      match Native_watcher.refresh watcher ~paths with
      | Error message -> Some (message, roots, current)
      | Ok () ->
        let after_refresh = snapshot digest_cache roots in
        if after_refresh <> current then native_reconcile watcher roots current
        else native_loop watcher roots after_refresh
  in
  with_signal_handlers (fun _ -> stop ()) (fun () ->
    let roots, _ = watch_context ~root ~prod in
    let before_build = snapshot digest_cache roots in
    build ~poll;
    let roots, paths = watch_context ~root ~prod in
    match Native_watcher.create ~paths with
    | Error message ->
      native_fallback message;
      polling_loop roots before_build
    | Ok watcher ->
      let fallback =
        Fun.protect
          (fun () -> native_reconcile watcher roots before_build)
          ~finally:(fun () -> Native_watcher.close watcher)
      in
      Option.iter
        (fun (message, roots, previous) ->
          native_fallback message;
          polling_loop roots previous)
        fallback)

let run ~root ~prod ~clear_screen ~show_progress ~build =
  Build_lock.with_watch root (fun watch_lock ->
    run_locked ~root ~prod ~clear_screen ~show_progress ~build ~watch_lock)
