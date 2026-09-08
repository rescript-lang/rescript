type wait_result =
  | Changed
  | Stopped
  | Failed of string

type watch_path = {
  directory: string;
  recursive: bool;
}

type t = {
  loop: Luv.Loop.t;
  timer: Luv.Timer.t;
  mutable handles: (string * Luv.FS_event.t) list;
  mutable changed: bool;
  mutable stopped: bool;
  mutable error: string option;
}

let error_message error =
  Printf.sprintf "%s: %s" (Luv.Error.err_name error) (Luv.Error.strerror error)

let directories_under paths =
  let visited = Hashtbl.create 64 in
  let rec walk acc directory =
    try
      let canonical = Unix.realpath directory in
      if Hashtbl.mem visited canonical then acc
      else (
        Hashtbl.add visited canonical ();
        Sys.readdir canonical |> Array.to_list
        |> List.fold_left
             (fun acc name ->
               if List.mem name ["lib"; "node_modules"; ".git"; "_build"] then
                 acc
               else
                 let path = Filename.concat canonical name in
                 try
                   let stat = Unix.stat path in
                   if stat.Unix.st_kind = Unix.S_DIR then walk acc path else acc
                 with Sys_error _ | Unix.Unix_error _ -> acc)
             (canonical :: acc))
    with Sys_error _ | Unix.Unix_error _ -> acc
  in
  paths
  |> List.fold_left
       (fun directories path ->
         if path.recursive then walk directories path.directory
         else
           try Unix.realpath path.directory :: directories
           with Sys_error _ | Unix.Unix_error _ -> directories)
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
  close_fs_handles watcher.loop (List.map snd watcher.handles);
  watcher.handles <- []

let install_handles watcher directories =
  let existing = Hashtbl.create (List.length watcher.handles) in
  List.iter
    (fun (directory, _) -> Hashtbl.add existing directory ())
    watcher.handles;
  let added = ref [] in
  let install directory =
    if not (Hashtbl.mem existing directory) then
      match Luv.FS_event.init ~loop:watcher.loop () with
      | Error error -> watcher.error <- Some (error_message error)
      | Ok handle ->
        added := (directory, handle) :: !added;
        Luv.FS_event.start handle directory (function
          | Ok _ ->
            watcher.changed <- true;
            Luv.Loop.stop watcher.loop
          | Error error ->
            watcher.error <- Some (error_message error);
            Luv.Loop.stop watcher.loop)
  in
  List.iter install directories;
  watcher.handles <- watcher.handles @ List.rev !added;
  match watcher.error with
  | None -> Ok ()
  | Some message -> Error message

let create ~paths =
  match Luv.Loop.init () with
  | Error error -> Error (error_message error)
  | Ok loop -> (
    match Luv.Timer.init ~loop () with
    | Error error ->
      ignore (Luv.Loop.close loop);
      Error (error_message error)
    | Ok timer ->
      let watcher =
        {loop; timer; handles = []; changed = false; stopped = false; error = None}
      in
      match install_handles watcher (directories_under paths) with
      | Ok () -> Ok watcher
      | Error _ as error ->
        remove_handles watcher;
        close_timer loop timer;
        ignore (Luv.Loop.close loop);
        error)

let wait watcher ~keep_running =
  watcher.changed <- false;
  watcher.stopped <- false;
  let check_running () =
    if not (keep_running ()) then (
      watcher.stopped <- true;
      Luv.Loop.stop watcher.loop)
  in
  (match Luv.Timer.start ~repeat:100 watcher.timer 100 check_running with
  | Ok () -> ()
  | Error error -> watcher.error <- Some (error_message error));
  while
    (not watcher.changed) && not watcher.stopped
    && Option.is_none watcher.error
  do
    ignore (Luv.Loop.run ~loop:watcher.loop ~mode:`ONCE ())
  done;
  ignore (Luv.Timer.stop watcher.timer);
  match watcher.error with
  | Some message -> Failed message
  | None -> if watcher.stopped then Stopped else Changed

let refresh watcher ~paths =
  let directories = directories_under paths in
  let desired = Hashtbl.create (List.length directories) in
  List.iter (fun directory -> Hashtbl.add desired directory ()) directories;
  let kept, removed =
    List.partition
      (fun (directory, _) -> Hashtbl.mem desired directory)
      watcher.handles
  in
  close_fs_handles watcher.loop (List.map snd removed);
  watcher.handles <- kept;
  watcher.error <- None;
  install_handles watcher directories

let close watcher =
  remove_handles watcher;
  ignore (Luv.Timer.stop watcher.timer);
  close_timer watcher.loop watcher.timer;
  ignore (Luv.Loop.close watcher.loop)

module For_test = struct
  let handle_count watcher = List.length watcher.handles
end
