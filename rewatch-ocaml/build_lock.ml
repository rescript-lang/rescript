type watch = {path: string; pid: string}

let read_owner_contents path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

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
  let restore_signals = Platform.defer_termination_signals () in
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
      ~finally:(fun () -> File_util.remove_file path)
      (fun () ->
        restore_signals ();
        action path)
  with exception_raised ->
    Option.iter close_out_noerr !channel;
    Option.iter File_util.remove_file !candidate;
    let exception_raised =
      try
        restore_signals ();
        exception_raised
      with signal_exception -> signal_exception
    in
    raise exception_raised

let clear_stale ?poll ~candidate path =
  let takeover = path ^ ".takeover" in
  try
    Unix.link candidate takeover;
    Fun.protect
      ~finally:(fun () -> File_util.remove_file takeover)
      (fun () ->
        match read_owner path with
        | Some owner when not (valid_owner owner) -> raise (malformed_error ())
        | Some owner when process_is_active ?poll owner -> ()
        | _ -> File_util.remove_file path);
    true
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    (match read_owner takeover with
    | Some owner when process_is_active ?poll owner -> ()
    | _ -> File_util.remove_file takeover);
    false

let restore_after_exception restore_signals exception_raised =
  try
    restore_signals ();
    exception_raised
  with signal_exception -> signal_exception

let unlink_existing path =
  try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let release_owned path pid =
  if read_owner_for_release path = Some pid then unlink_existing path

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
        let restore_signals = Platform.defer_termination_signals () in
        let linked =
          try
            Unix.link candidate path;
            true
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> false
          | exception_raised ->
            raise (restore_after_exception restore_signals exception_raised)
        in
        if linked then
          let released = ref false in
          let release () =
            if not !released then (
              release_owned path pid;
              released := true)
          in
          Fun.protect ~finally:release (fun () ->
              unlink_existing candidate;
              restore_signals ();
              action ~release)
        else (
          restore_signals ();
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
        let restore_signals = Platform.defer_termination_signals () in
        let linked =
          try
            Unix.link candidate path;
            true
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> false
          | exception_raised ->
            raise (restore_after_exception restore_signals exception_raised)
        in
        if linked then
          let watch = {path; pid} in
          Fun.protect
            ~finally:(fun () -> release_owned path pid)
            (fun () ->
              unlink_existing candidate;
              restore_signals ();
              action watch)
        else (
          restore_signals ();
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

let is_owned watch = read_owner watch.path = Some watch.pid
