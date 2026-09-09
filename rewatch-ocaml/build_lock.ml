open Build_artifacts

type watch = {path: string; pid: string}

let read_owner path =
  try
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      Some (really_input_string channel (in_channel_length channel)))
  with Sys_error _ -> None

let valid_owner value =
  match Int64.of_string_opt value with
  | Some pid -> pid >= 0L && pid <= 0xffff_ffffL
  | None -> false

let malformed_error () =
  Project_context.Error
    "Could not start Rescript build: Could not parse lockfile PID\n  (try removing it and running the command again)"

let process_is_active value =
  Platform.process_is_active value ~run:(fun program args ->
    try
      let result =
        Process.run ~cwd:(Filename.get_temp_dir_name ()) program args
      in
      Some (result.Process.status, result.stdout)
    with
    | Process.Error _ | Unix.Unix_error _ | Sys_error _ -> None)

let write_candidate ~lock_dir prefix pid =
  let candidate = Filename.temp_file ~temp_dir:lock_dir prefix ".tmp" in
  let channel = open_out candidate in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel pid);
  candidate

let clear_stale ~candidate path =
  let takeover = path ^ ".takeover" in
  try
    Unix.link candidate takeover;
    Fun.protect
      ~finally:(fun () -> remove_file takeover)
      (fun () ->
        match read_owner path with
        | Some owner when not (valid_owner owner) -> raise (malformed_error ())
        | Some owner when process_is_active owner -> ()
        | _ -> remove_file path);
    true
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    (match read_owner takeover with
    | Some owner when process_is_active owner -> ()
    | _ -> remove_file takeover);
    false

let acquire_build root =
  let lock_dir = Filename.concat root "lib" in
  ensure_dir lock_dir;
  let path = Filename.concat lock_dir "build.lock" in
  let pid = string_of_int (Unix.getpid ()) in
  let candidate = write_candidate ~lock_dir ".build-lock-" pid in
  let rec acquire attempts =
    if attempts = 0 then
      raise
        (Project_context.Error
           "Timed out waiting for another ReScript build to finish");
    try Unix.link candidate path
    with Unix.Unix_error (Unix.EEXIST, _, _) -> (
      match read_owner path with
      | Some owner when not (valid_owner owner) -> raise (malformed_error ())
      | Some owner when process_is_active owner ->
        if attempts = 1200 then print_endline "Waiting for other build to finish...";
        ignore (Unix.select [] [] [] 0.05);
        acquire (attempts - 1)
      | _ ->
        if not (clear_stale ~candidate path) then
          ignore (Unix.select [] [] [] 0.05);
        acquire (attempts - 1))
  in
  Fun.protect ~finally:(fun () -> remove_file candidate) (fun () -> acquire 1200);
  let released = ref false in
  fun () ->
    if not !released then (
      if read_owner path = Some pid then remove_file path;
      released := true)

let acquire_watch root =
  let lock_dir = Filename.concat root "lib" in
  ensure_dir lock_dir;
  let path = Filename.concat lock_dir "watch.lock" in
  let pid = string_of_int (Unix.getpid ()) in
  let candidate = write_candidate ~lock_dir ".watch-lock-" pid in
  let rec acquire attempts =
    if attempts = 0 then
      raise
        (Project_context.Error
           "Timed out recovering a stale ReScript watch lock");
    try Unix.link candidate path
    with Unix.Unix_error (Unix.EEXIST, _, _) -> (
      match read_owner path with
      | Some owner when not (valid_owner owner) -> raise (malformed_error ())
      | Some owner when process_is_active owner ->
        raise
          (Project_context.Error
             (Printf.sprintf
                "Could not start Rescript build: A ReScript build is already running. The process ID (PID) is %s"
                owner))
      | _ ->
        if not (clear_stale ~candidate path) then
          ignore (Unix.select [] [] [] 0.01);
        acquire (attempts - 1))
  in
  Fun.protect ~finally:(fun () -> remove_file candidate) (fun () -> acquire 1000);
  {path; pid}

let is_owned watch = read_owner watch.path = Some watch.pid
let release watch = if is_owned watch then remove_file watch.path
