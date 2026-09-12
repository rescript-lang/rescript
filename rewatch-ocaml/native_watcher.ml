type change_kind = Content | Structural
type change = {path: string option; kind: change_kind}

type wait_result = Changed of change list | Stopped | Failed of string

type watch_path = {directory: string; recursive: bool}

type watched_directory = {
  directory: string;
  identity: string;
  handle: Luv.FS_event.t;
}

type t = {
  loop: Luv.Loop.t;
  timer: Luv.Timer.t;
  mutable handles: watched_directory list;
  mutable changes: change list;
  mutable stopped: bool;
  mutable error: string option;
}

let error_message error =
  Printf.sprintf "%s: %s" (Luv.Error.err_name error) (Luv.Error.strerror error)

let is_compiler_artifact_directory path =
  let name = Filename.basename path in
  (name = "bs" || name = "ocaml")
  && Filename.basename (Filename.dirname path) = "lib"

let directories_under paths =
  let visited = Hashtbl.create 64 in
  let rec walk acc directory =
    match Platform.canonicalize_path directory with
    | canonical -> (
      if Hashtbl.mem visited canonical then acc
      else
        let entries =
          try Some (File_util.directory_entries canonical)
          with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None
        in
        match entries with
        | None -> acc
        | Some entries ->
          Hashtbl.add visited canonical ();
          entries
          |> List.fold_left
               (fun acc name ->
                 let path = Filename.concat canonical name in
                 if is_compiler_artifact_directory path then acc
                 else
                   match Unix.stat path with
                   | stat ->
                     if stat.Unix.st_kind = Unix.S_DIR then walk acc path
                     else acc
                   | exception
                       Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
                     acc)
               (canonical :: acc))
    | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> acc
  in
  paths
  |> List.fold_left
       (fun directories path ->
         if path.recursive then walk directories path.directory
         else
           match Platform.canonicalize_path path.directory with
           | canonical -> canonical :: directories
           | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
             directories)
       []
  |> List.sort_uniq String.compare

let close_fs_handles loop handles =
  let pending = ref 0 in
  List.iter
    (fun handle ->
      ignore (Luv.FS_event.stop handle);
      if not (Luv.Handle.is_closing handle) then (
        incr pending;
        Luv.Handle.close handle (fun () -> decr pending)))
    handles;
  while !pending > 0 do
    ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ())
  done

let close_timer loop timer =
  if not (Luv.Handle.is_closing timer) then (
    let closed = ref false in
    Luv.Handle.close timer (fun () -> closed := true);
    while not !closed do
      ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ())
    done)

let remove_handles watcher =
  close_fs_handles watcher.loop
    (List.map (fun watched -> watched.handle) watcher.handles);
  watcher.handles <- []

let close watcher =
  remove_handles watcher;
  ignore (Luv.Timer.stop watcher.timer);
  close_timer watcher.loop watcher.timer;
  ignore (Luv.Loop.close watcher.loop)

let directory_identity directory =
  try
    let metadata = Unix.stat directory in
    Ok (Platform.directory_identity ~path:directory metadata)
  with (Sys_error _ | Unix.Unix_error _) as error ->
    Error
      (Printf.sprintf "could not identify watched directory %s: %s" directory
         (Printexc.to_string error))

let identify_directories ~directory_identity directories =
  let rec identify identified = function
    | [] -> Ok (List.rev identified)
    | directory :: rest -> (
      match directory_identity directory with
      | Ok identity -> identify ((directory, identity) :: identified) rest
      | Error _ as error -> error)
  in
  identify [] directories

let install_handles watcher identified_directories =
  let existing = Hashtbl.create (List.length watcher.handles) in
  List.iter
    (fun watched -> Hashtbl.add existing watched.directory ())
    watcher.handles;
  let install (directory, identity) =
    if not (Hashtbl.mem existing directory) then
      match Luv.FS_event.init ~loop:watcher.loop () with
      | Error error -> watcher.error <- Some (error_message error)
      | Ok handle ->
        watcher.handles <- {directory; identity; handle} :: watcher.handles;
        Luv.FS_event.start handle directory (function
          | Ok (filename, events) ->
            let path =
              match filename with
              | None | Some "" -> None
              | Some filename ->
                Some
                  (if Filename.is_relative filename then
                     Filename.concat directory filename
                   else filename)
            in
            let kind =
              if List.mem `CHANGE events && not (List.mem `RENAME events) then
                Content
              else Structural
            in
            watcher.changes <- {path; kind} :: watcher.changes;
            Luv.Loop.stop watcher.loop
          | Error error ->
            watcher.error <- Some (error_message error);
            Luv.Loop.stop watcher.loop)
  in
  List.iter install identified_directories;
  match watcher.error with
  | None -> Ok ()
  | Some message -> Error message

let create_with_directory_identity ~directory_identity ~paths =
  match Luv.Loop.init () with
  | Error error -> Error (error_message error)
  | Ok loop -> (
    match Luv.Timer.init ~loop () with
    | Error error ->
      ignore (Luv.Loop.close loop);
      Error (error_message error)
    | Ok timer -> (
      let watcher =
        {loop; timer; handles = []; changes = []; stopped = false; error = None}
      in
      let fail message =
        try
          close watcher;
          Error message
        with cleanup_error ->
          Error
            (Printf.sprintf "%s (native watcher cleanup failed: %s)" message
               (Printexc.to_string cleanup_error))
      in
      match
        try
          match
            identify_directories ~directory_identity (directories_under paths)
          with
          | Error _ as error -> error
          | Ok directories -> install_handles watcher directories
        with error -> Error (Printexc.to_string error)
      with
      | Ok () -> Ok watcher
      | Error message -> fail message))

let create = create_with_directory_identity ~directory_identity

let wait watcher ~keep_running =
  watcher.stopped <- not (keep_running ());
  (* Events can arrive while refresh closes handles and pumps the libuv loop.
     Consume the event that wakes this call, but never erase one already queued
     by reconciliation before the next wait begins. A failed handle or an
     external stop must take precedence because another reconciliation could
     otherwise erase the condition that requires native watch to end. *)
  let ready_result () =
    match watcher.error with
    | Some message -> Some (Failed message)
    | None when watcher.stopped -> Some Stopped
    | None when watcher.changes <> [] ->
      let changes = List.rev watcher.changes in
      watcher.changes <- [];
      Some (Changed changes)
    | None -> None
  in
  match ready_result () with
  | Some result -> result
  | None -> (
    let check_running () =
      try
        if not (keep_running ()) then (
          watcher.stopped <- true;
          Luv.Loop.stop watcher.loop)
      with error ->
        watcher.error <- Some (Printexc.to_string error);
        Luv.Loop.stop watcher.loop
    in
    (match Luv.Timer.start ~repeat:100 watcher.timer 100 check_running with
    | Ok () -> ()
    | Error error -> watcher.error <- Some (error_message error));
    while
      watcher.changes = [] && (not watcher.stopped)
      && Option.is_none watcher.error
    do
      ignore (Luv.Loop.run ~loop:watcher.loop ~mode:`ONCE ())
    done;
    ignore (Luv.Timer.stop watcher.timer);
    match ready_result () with
    | Some result -> result
    | None -> Failed "native watcher loop stopped without a wakeup condition")

let drain watcher =
  (* The first callback stops the loop so the build can start promptly. Pump
     already-ready callbacks after the debounce window to keep one editor save
     together without waiting for another build cycle. *)
  let rec pump remaining =
    if remaining > 0 then (
      let changes = watcher.changes in
      ignore (Luv.Loop.run ~loop:watcher.loop ~mode:`NOWAIT ());
      if watcher.changes != changes then pump (remaining - 1))
  in
  pump 1024;
  let changes = List.rev watcher.changes in
  watcher.changes <- [];
  changes

let watches_directory watcher path =
  List.exists (fun watched -> watched.directory = path) watcher.handles

let refresh_with_directory_identity ~directory_identity watcher ~paths =
  (* Only failures from work completed before this refresh are stale. Clear
     them before pumping close callbacks so a newly reported handle failure is
     preserved and makes the caller fall back to polling. *)
  watcher.error <- None;
  let directories = directories_under paths in
  match identify_directories ~directory_identity directories with
  | Error _ as error -> error
  | Ok identified_directories ->
    let desired = Hashtbl.create (List.length directories) in
    List.iter
      (fun (directory, identity) -> Hashtbl.add desired directory identity)
      identified_directories;
    let kept, removed =
      List.partition
        (fun watched ->
          Hashtbl.find_opt desired watched.directory = Some watched.identity)
        watcher.handles
    in
    close_fs_handles watcher.loop
      (List.map (fun watched -> watched.handle) removed);
    watcher.handles <- kept;
    install_handles watcher identified_directories

let refresh = refresh_with_directory_identity ~directory_identity

module For_test = struct
  let create_with_directory_identity = create_with_directory_identity
  let refresh_with_directory_identity = refresh_with_directory_identity
  let handle_count watcher = List.length watcher.handles
  let directory_identity watcher directory =
    watcher.handles
    |> List.find_opt (fun watched -> watched.directory = directory)
    |> Option.map (fun watched -> watched.identity)

  let queue_change watcher =
    watcher.changes <- {path = None; kind = Structural} :: watcher.changes

  let queue_error watcher message = watcher.error <- Some message
end
