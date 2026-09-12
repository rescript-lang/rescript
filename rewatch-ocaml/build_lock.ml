type watch = {path: string; pid: string}

let read_owner_contents = File_util.read_file

let read_owner path =
  try Some (read_owner_contents path) with Sys_error _ -> None

let read_owner_for_release path =
  (* Lock release must distinguish a missing file from an unreadable one so a
     transient sharing or permission error cannot be mistaken for successful
     cleanup. stat confirms absence after the ordinary owner read fails. *)
  try Some (read_owner_contents path)
  with Sys_error _ as read_error ->
    let exists =
      try
        ignore (Unix.stat path);
        true
      with Unix.Unix_error (Unix.ENOENT, _, _) -> false
    in
    if exists then raise read_error else None

let valid_owner value =
  match Int64.of_string_opt value with
  | Some pid -> pid >= 0L && pid <= 0xffff_ffffL
  | None -> false

let malformed_error () =
  Project_context.Error
    "Could not start Rescript build: Could not parse lockfile PID\n\
    \  (try removing it and running the command again)"

let process_is_active ?poll value =
  Platform.process_is_active value ~run:(fun program args ->
      try
        let result =
          Process.run ?poll ~cwd:(Filename.get_temp_dir_name ()) program args
        in
        Some (result.Process.status, result.stdout)
      with Process.Error _ | Unix.Unix_error _ | Sys_error _ -> None)

let with_candidate ~lock_dir prefix pid action =
  (* Termination is deferred until candidate cleanup has an owner because the
     watcher's signal handlers raise asynchronous exceptions. Without this
     protected setup, a signal could leave the temporary file or its output
     channel behind between two otherwise ordinary OCaml expressions. *)
  let deferred_signals = Signal_restore.create ~defer:true in
  let candidate = ref None in
  let channel = ref None in
  try
    let path, output =
      Filename.open_temp_file ~temp_dir:lock_dir prefix ".tmp"
    in
    candidate := Some path;
    channel := Some output;
    output_string output pid;
    close_out output;
    channel := None;
    Fun.protect
      ~finally:(fun () -> File_util.remove_file_best_effort path)
      (fun () ->
        Signal_restore.restore deferred_signals;
        action path)
  with exception_raised ->
    Option.iter close_out_noerr !channel;
    Option.iter File_util.remove_file_best_effort !candidate;
    raise
      (Signal_restore.exception_after_restore deferred_signals exception_raised)

let clear_stale ?poll ~candidate path =
  let takeover = path ^ ".takeover" in
  try
    Unix.link candidate takeover;
    Fun.protect
      ~finally:(fun () -> File_util.remove_file_best_effort takeover)
      (fun () ->
        match read_owner path with
        | Some owner when not (valid_owner owner) -> raise (malformed_error ())
        | Some owner when process_is_active ?poll owner -> ()
        | _ -> File_util.remove_file_best_effort path);
    true
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    (match read_owner takeover with
    | Some owner when process_is_active ?poll owner -> ()
    | _ -> File_util.remove_file_best_effort takeover);
    false

let unlink_existing path =
  try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let release_owned path pid =
  if read_owner_for_release path = Some pid then unlink_existing path

type owned_lock = {path: string; pid: string; mutable released: bool}

let release lock =
  if not lock.released then (
    release_owned lock.path lock.pid;
    lock.released <- true)

let attempt_link ~candidate ~path =
  let deferred_signals = Signal_restore.create ~defer:true in
  let linked =
    try
      Unix.link candidate path;
      true
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> false
    | exception_raised ->
      raise
        (Signal_restore.exception_after_restore deferred_signals
           exception_raised)
  in
  (linked, deferred_signals)

let with_acquired ~candidate ~path ~pid ~deferred_signals action =
  let lock = {path; pid; released = false} in
  Fun.protect
    ~finally:(fun () -> release lock)
    (fun () ->
      unlink_existing candidate;
      Signal_restore.restore deferred_signals;
      action lock)

let retry_delay poll =
  (try ignore (Unix.select [] [] [] 0.05)
   with Unix.Unix_error (Unix.EINTR, _, _) -> ());
  poll ()

let with_build ?(poll = fun () -> ()) root action =
  let lock_dir = Filename.concat root "lib" in
  File_util.ensure_dir lock_dir;
  let path = Filename.concat lock_dir "build.lock" in
  let pid = string_of_int (Unix.getpid ()) in
  with_candidate ~lock_dir ".build-lock-" pid (fun candidate ->
      let rec acquire attempts =
        poll ();
        if attempts = 0 then
          raise
            (Project_context.Error
               "Timed out waiting for another ReScript build to finish");
        let linked, deferred_signals = attempt_link ~candidate ~path in
        if linked then
          with_acquired ~candidate ~path ~pid ~deferred_signals (fun lock ->
              action ~release:(fun () -> release lock))
        else (
          Signal_restore.restore deferred_signals;
          match read_owner path with
          | Some owner when not (valid_owner owner) ->
            raise (malformed_error ())
          | Some owner when process_is_active ~poll owner ->
            if attempts = 1200 then
              print_endline "Waiting for other build to finish...";
            retry_delay poll;
            acquire (attempts - 1)
          | _ ->
            if not (clear_stale ~poll ~candidate path) then retry_delay poll;
            acquire (attempts - 1))
      in
      acquire 1200)

let with_watch root action =
  let lock_dir = Filename.concat root "lib" in
  File_util.ensure_dir lock_dir;
  let path = Filename.concat lock_dir "watch.lock" in
  let pid = string_of_int (Unix.getpid ()) in
  with_candidate ~lock_dir ".watch-lock-" pid (fun candidate ->
      let rec acquire attempts =
        if attempts = 0 then
          raise
            (Project_context.Error
               "Timed out recovering a stale ReScript watch lock");
        let linked, deferred_signals = attempt_link ~candidate ~path in
        if linked then
          with_acquired ~candidate ~path ~pid ~deferred_signals (fun _ ->
              action {path; pid})
        else (
          Signal_restore.restore deferred_signals;
          match read_owner path with
          | Some owner when not (valid_owner owner) ->
            raise (malformed_error ())
          | Some owner when process_is_active owner ->
            raise
              (Project_context.Error
                 (Printf.sprintf
                    "Could not start Rescript build: A ReScript build is \
                     already running. The process ID (PID) is %s"
                    owner))
          | _ ->
            if not (clear_stale ~candidate path) then
              ignore (Unix.select [] [] [] 0.01);
            acquire (attempts - 1))
      in
      acquire 1000)

let is_owned (watch : watch) = read_owner watch.path = Some watch.pid
