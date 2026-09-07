type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary_log ?temp_dir stream =
  Filename.temp_file ?temp_dir (".rewatch-ocaml-" ^ stream ^ "-") ".log"

let resolve_program ~cwd program =
  if (not (Filename.is_relative program)) || Filename.dirname program <> "."
  then program
  else
    let path_separator = if Sys.win32 then ';' else ':' in
    let extensions =
      if not Sys.win32 || Filename.extension program <> "" then [""]
      else
        Sys.getenv_opt "PATHEXT"
        |> Option.value ~default:".COM;.EXE;.BAT;.CMD"
        |> String.split_on_char ';'
    in
    let path_directories =
      Sys.getenv_opt "PATH" |> Option.value ~default:""
      |> String.split_on_char path_separator
    in
    let directories = if Sys.win32 then cwd :: path_directories else path_directories in
    directories
    |> List.find_map (fun directory ->
         let directory =
           let directory = String.trim directory in
           let length = String.length directory in
           let directory =
             if
               length >= 2 && directory.[0] = '"'
               && directory.[length - 1] = '"'
             then String.sub directory 1 (length - 2)
             else directory
           in
           if directory = "" then cwd
           else if Filename.is_relative directory then
             Filename.concat cwd directory
           else directory
         in
         extensions
         |> List.find_map (fun extension ->
              let candidate = Filename.concat directory (program ^ extension) in
              let runnable =
                try
                  (Unix.stat candidate).Unix.st_kind = Unix.S_REG
                  && (Sys.win32
                     || try
                          Unix.access candidate [Unix.X_OK];
                          true
                        with Unix.Unix_error _ -> false)
                with Unix.Unix_error _ -> false
              in
              if runnable then Some candidate else None))
    |> Option.value ~default:program

let spawn ~env ~cwd ~program ~args ~stdout ~stderr =
  let program = resolve_program ~cwd program in
  let program, args =
    if
      Sys.win32
      && List.mem
           (Filename.extension program |> String.lowercase_ascii)
           [".bat"; ".cmd"]
    then
      let command = Filename.quote_command program args in
      ( resolve_program ~cwd "cmd.exe",
        ["/D"; "/V:OFF"; "/S"; "/C"; command] )
    else (program, args)
  in
  let arguments = program :: args in
  if Sys.win32 then
    Spawn.spawn ?env ~cwd:(Spawn.Working_dir.Path cwd) ~prog:program
      ~argv:arguments ~stdout ~stderr ()
  else
    Spawn.spawn ?env ~cwd:(Spawn.Working_dir.Path cwd) ~prog:program
      ~argv:arguments ~stdout ~stderr ~setpgid:Spawn.Pgid.new_process_group ()

let signal_process_tree pid signal =
  let target = if Sys.win32 then pid else -pid in
  try Unix.kill target signal with Unix.Unix_error _ -> ()

let defer_termination_signals () =
  if not Sys.win32 then
    let previous =
      Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigterm]
    in
    fun () -> ignore (Unix.sigprocmask Unix.SIG_SETMASK previous)
  else
    let pending = ref [] in
    let defer signal =
      if not (List.mem signal !pending) then pending := signal :: !pending
    in
    let previous_int = Sys.signal Sys.sigint (Sys.Signal_handle defer) in
    let previous_term =
      try Sys.signal Sys.sigterm (Sys.Signal_handle defer)
      with exn ->
        ignore (Sys.signal Sys.sigint previous_int);
        raise exn
    in
    let restored = ref false in
    let dispatch signal behavior =
      match behavior with
      | Sys.Signal_ignore -> ()
      | Sys.Signal_handle handler -> handler signal
      | Sys.Signal_default -> raise Sys.Break
    in
    fun () ->
      if not !restored then (
        restored := true;
        ignore (Sys.signal Sys.sigint previous_int);
        ignore (Sys.signal Sys.sigterm previous_term);
        List.rev !pending
        |> List.iter (fun signal ->
             dispatch signal
               (if signal = Sys.sigint then previous_int else previous_term)))

let run ?env ~cwd program args =
  let restore_signals = defer_termination_signals () in
  let child_pid = ref None in
  let stdout_path = ref None in
  let stderr_path = ref None in
  let stdout_fd = ref None in
  let stderr_fd = ref None in
  let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> () in
  let remove_log path = try Sys.remove path with Sys_error _ -> () in
  let cleanup () =
    Option.iter close_fd !stdout_fd;
    Option.iter close_fd !stderr_fd;
    stdout_fd := None;
    stderr_fd := None;
    Option.iter remove_log !stdout_path;
    Option.iter remove_log !stderr_path
  in
  try
    let stdout_log = temporary_log "stdout" in
    stdout_path := Some stdout_log;
    let stderr_log = temporary_log "stderr" in
    stderr_path := Some stderr_log;
    let out = Unix.openfile stdout_log [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
    stdout_fd := Some out;
    let err = Unix.openfile stderr_log [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
    stderr_fd := Some err;
    let pid =
      spawn ~env ~cwd ~program ~args ~stdout:out ~stderr:err
    in
    child_pid := Some pid;
    close_fd out;
    stdout_fd := None;
    close_fd err;
    stderr_fd := None;
    restore_signals ();
    let _, status = Unix.waitpid [] pid in
    child_pid := None;
    let stdout = read_file stdout_log in
    let stderr = read_file stderr_log in
    cleanup ();
    {status; stdout; stderr}
  with exn ->
    Option.iter
      (fun pid ->
        signal_process_tree pid Sys.sigkill;
        try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
      !child_pid;
    cleanup ();
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let succeeded result = result.status = Unix.WEXITED 0

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

(* Each child writes to private files, so diagnostics cannot interleave.  The
   scheduler refills a slot as soon as any child exits while returning results
   in input order. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

let run_parallel ?temp_dir ?(max_jobs = default_max_jobs) jobs =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  let indexed = List.mapi (fun index job -> (index, job)) jobs in
  let results = Array.make (List.length jobs) None in
  let active = ref [] in
  let remove_log path = try Sys.remove path with Sys_error _ -> () in
  let cleanup_children () =
    let remove_child_logs (_, _, stdout_path, stderr_path) =
      remove_log stdout_path;
      remove_log stderr_path
    in
    let children = !active in
    let signal_group signal (_, pid, _, _) = signal_process_tree pid signal in
    let graceful_signal = if Sys.win32 then Sys.sigkill else Sys.sigterm in
    List.iter (signal_group graceful_signal) children;
    let deadline = Unix.gettimeofday () +. 0.25 in
    let rec reap_until_deadline children =
      let remaining =
        List.filter
          (fun ((_, pid, _, _) as child) ->
            try
              match Unix.waitpid [Unix.WNOHANG] pid with
              | 0, _ -> true
              | _ ->
                remove_child_logs child;
                false
            with
            | Unix.Unix_error (Unix.EINTR, _, _) -> true
            | Unix.Unix_error (Unix.ECHILD, _, _) ->
              remove_child_logs child;
              false)
          children
      in
      if remaining <> [] && Unix.gettimeofday () < deadline then (
        ignore (Unix.select [] [] [] 0.01);
        reap_until_deadline remaining)
      else remaining
    in
    let remaining = reap_until_deadline children in
    (* A direct child may have exited while a PPX/helper in its process group
       remains alive, so escalate every original group rather than only the
       direct children that still need reaping. *)
    List.iter (signal_group Sys.sigkill) children;
    List.iter
      (fun ((_, pid, _, _) as child) ->
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
        remove_child_logs child)
      remaining;
    active := []
  in
  let launch (index, job) =
    let restore_signals = defer_termination_signals () in
    let stdout_path = ref None in
    let stderr_path = ref None in
    let stdout_fd = ref None in
    let stderr_fd = ref None in
    try
      let stdout_log = temporary_log ?temp_dir "stdout" in
      stdout_path := Some stdout_log;
      let stderr_log = temporary_log ?temp_dir "stderr" in
      stderr_path := Some stderr_log;
      let out =
        Unix.openfile stdout_log [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      in
      stdout_fd := Some out;
      let err =
        Unix.openfile stderr_log [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      in
      stderr_fd := Some err;
      let pid =
        spawn ~env:None ~cwd:job.cwd ~program:job.program ~args:job.args
          ~stdout:out ~stderr:err
      in
      active := (index, pid, stdout_log, stderr_log) :: !active;
      (try Unix.close out with Unix.Unix_error _ -> ());
      stdout_fd := None;
      (try Unix.close err with Unix.Unix_error _ -> ());
      stderr_fd := None;
      restore_signals ()
    with exn ->
      Option.iter
        (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        !stdout_fd;
      Option.iter
        (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        !stderr_fd;
      Option.iter remove_log !stdout_path;
      Option.iter remove_log !stderr_path;
      let exn = try restore_signals (); exn with signal_exn -> signal_exn in
      raise exn
  in
  let rec fill slots queued =
    if slots = 0 then queued
    else
      match queued with
      | [] -> []
      | job :: rest ->
        launch job;
        fill (slots - 1) rest
  in
  let rec schedule queued =
    let queued = fill (max_jobs - List.length !active) queued in
    match !active with
    | [] -> ()
    | _ ->
      let rec wait_for_active = function
        | [] ->
          ignore (Unix.select [] [] [] 0.0005);
          wait_for_active !active
        | ((_, pid, _, _) as child) :: rest -> (
          match Unix.waitpid [Unix.WNOHANG] pid with
          | 0, _ -> wait_for_active rest
          | _, status -> (child, status))
      in
      let (index, pid, stdout_path, stderr_path), status =
        wait_for_active !active
      in
      active :=
        List.filter (fun (_, active_pid, _, _) -> active_pid <> pid) !active;
      let result =
        Fun.protect
          ~finally:(fun () ->
            remove_log stdout_path;
            remove_log stderr_path)
          (fun () ->
            {
              status;
              stdout = read_file stdout_path;
              stderr = read_file stderr_path;
            })
      in
      results.(index) <- Some result;
      schedule queued
  in
  try
    schedule indexed;
    Array.to_list results
    |> List.map (function
         | Some result -> result
         | None -> raise (Error "subprocess result was not collected"))
  with exn ->
    cleanup_children ();
    raise exn
