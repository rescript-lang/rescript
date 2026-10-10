type watch = {path: string; pid: string}

let read_owner_contents = File_util.read_file

let read_owner path =
  try Some (read_owner_contents path)
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None

let read_owner_for_release = read_owner

let valid_owner value =
  match Int64.of_string_opt value with
  | Some pid -> pid >= 0L && pid <= 0xffff_ffffL
  | None -> false

let malformed_error () =
  Project_context.Error
    "Could not start Rescript build: Could not parse lockfile PID\n\
    \  (try removing it and running the command again)"

(* A writer that cannot link creates the lock and then writes its PID, as Rust
   rewatch always does, so an empty lock is usually still being written. One
   that stays empty belongs to a writer that died in between. *)
let being_written path =
  match Unix.stat path with
  | stats -> Unix.gettimeofday () -. stats.Unix.st_mtime < 5.0
  | exception Unix.Unix_error _ -> false

(* Linking a fully written candidate publishes the lock atomically. Some
   filesystems (FAT, exFAT, and some network or virtual-machine shares) have no
   hard links; there the lock is created exclusively and then written. Falling
   back on any other link error is safe because the exclusive create reports
   the existing lock or the real error itself. *)
let publish ~candidate ~path ~contents =
  try
    Unix.link candidate path;
    true
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> false
  | Unix.Unix_error _ -> (
    match
      Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o644
    with
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> false
    | descriptor ->
      (try
         Fun.protect
           ~finally:(fun () -> Unix.close descriptor)
           (fun () ->
             let length = String.length contents in
             if Unix.write_substring descriptor contents 0 length <> length then
               failwith ("short write to " ^ path))
       with error ->
         (try Unix.unlink path with Unix.Unix_error _ -> ());
         raise error);
      true)

let process_is_active ?poll value =
  Platform.process_is_active value ~run:(fun program args ->
      try
        let result =
          Process.run ?poll ~cwd:(Filename.get_temp_dir_name ()) program args
        in
        Some (result.Process.status, result.stdout)
      with Process.Error _ | Unix.Unix_error _ | Sys_error _ -> None)

let with_candidate ~lock_dir prefix pid action =
  let path, output = Filename.open_temp_file ~temp_dir:lock_dir prefix ".tmp" in
  Fun.protect
    ~finally:(fun () -> File_util.remove_file_best_effort path)
    (fun () ->
      (try
         output_string output pid;
         close_out output
       with exn ->
         close_out_noerr output;
         raise exn);
      action path)

let clear_stale ?poll ~candidate ~pid path =
  let takeover = path ^ ".takeover" in
  if publish ~candidate ~path:takeover ~contents:pid then (
    Fun.protect
      ~finally:(fun () -> File_util.remove_file_best_effort takeover)
      (fun () ->
        match read_owner path with
        | Some "" when being_written path -> ()
        | Some "" -> File_util.remove_file_best_effort path
        | Some owner when not (valid_owner owner) -> raise (malformed_error ())
        | Some owner when process_is_active ?poll owner -> ()
        | Some _ -> File_util.remove_file_best_effort path
        | None -> ());
    true)
  else (
    (match read_owner takeover with
    | Some "" when being_written takeover -> ()
    | Some owner when owner <> "" && process_is_active ?poll owner -> ()
    | _ -> File_util.remove_file_best_effort takeover);
    false)

let unlink_existing path =
  try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let release_owned path pid =
  if read_owner_for_release path = Some pid then unlink_existing path

type owned_lock = {path: string; pid: string; mutable released: bool}

let release lock =
  if not lock.released then (
    release_owned lock.path lock.pid;
    lock.released <- true)

let with_acquired ~candidate ~path ~pid action =
  let lock = {path; pid; released = false} in
  match
    unlink_existing candidate;
    action lock
  with
  | result ->
    release lock;
    result
  | exception original -> (
    match release lock with
    | () -> raise original
    | exception cleanup_error -> raise cleanup_error)

let retry_delay poll =
  (try ignore (Unix.select [] [] [] 0.05)
   with Unix.Unix_error (Unix.EINTR, _, _) -> ());
  poll ()

let with_build ?(poll = fun () -> ()) root action =
  let lock_dir = Filename.concat root "lib" in
  File_util.ensure_dir lock_dir;
  let path = Filename.concat lock_dir "build.lock" in
  let pid = string_of_int (Platform.current_process_id ()) in
  with_candidate ~lock_dir ".build-lock-" pid (fun candidate ->
      let rec acquire attempts =
        poll ();
        if attempts = 0 then
          raise
            (Project_context.Error
               "Timed out waiting for another ReScript build to finish");
        if publish ~candidate ~path ~contents:pid then
          with_acquired ~candidate ~path ~pid (fun lock ->
              action ~release:(fun () -> release lock))
        else
          match read_owner path with
          | Some "" when being_written path ->
            retry_delay poll;
            acquire (attempts - 1)
          | Some owner when owner <> "" && not (valid_owner owner) ->
            raise (malformed_error ())
          | Some owner when process_is_active ~poll owner ->
            if attempts = 1200 then (
              print_endline "Waiting for other build to finish...";
              flush stdout);
            retry_delay poll;
            acquire (attempts - 1)
          | _ ->
            if not (clear_stale ~poll ~candidate ~pid path) then
              retry_delay poll;
            acquire (attempts - 1)
      in
      acquire 1200)

let with_watch root action =
  let lock_dir = Filename.concat root "lib" in
  File_util.ensure_dir lock_dir;
  let path = Filename.concat lock_dir "watch.lock" in
  let pid = string_of_int (Platform.current_process_id ()) in
  with_candidate ~lock_dir ".watch-lock-" pid (fun candidate ->
      let rec acquire attempts =
        if attempts = 0 then
          raise
            (Project_context.Error
               "Timed out recovering a stale ReScript watch lock");
        if publish ~candidate ~path ~contents:pid then
          with_acquired ~candidate ~path ~pid (fun _ -> action {path; pid})
        else
          match read_owner path with
          | Some "" when being_written path ->
            ignore (Unix.select [] [] [] 0.01);
            acquire (attempts - 1)
          | Some owner when owner <> "" && not (valid_owner owner) ->
            raise (malformed_error ())
          | Some owner when process_is_active owner ->
            raise
              (Project_context.Error
                 (Printf.sprintf
                    "Could not start Rescript build: A ReScript build is \
                     already running. The process ID (PID) is %s"
                    owner))
          | _ ->
            if not (clear_stale ~candidate ~pid path) then
              ignore (Unix.select [] [] [] 0.01);
            acquire (attempts - 1)
      in
      acquire 1000)

let is_owned (watch : watch) = read_owner watch.path = Some watch.pid

module For_test = struct
  let publish = publish
end
