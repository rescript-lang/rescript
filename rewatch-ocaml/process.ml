type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary_log ~cwd stream =
  Filename.temp_file ~temp_dir:cwd (".rewatch-ocaml-" ^ stream ^ "-") ".log"

let run ~cwd program args =
  let stdout_path = temporary_log ~cwd "stdout" in
  let stderr_path = temporary_log ~cwd "stderr" in
  let child_pid = ref None in
  let stdout_fd =
    Unix.openfile stdout_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  in
  let cleanup () =
    (try Sys.remove stdout_path with Sys_error _ -> ());
    try Sys.remove stderr_path with Sys_error _ -> ()
  in
  try
    match Unix.fork () with
    | 0 -> (
      try
        Unix.chdir cwd;
        Unix.dup2 stdout_fd Unix.stdout;
        Unix.dup2 stderr_fd Unix.stderr;
        Unix.close stdout_fd;
        Unix.close stderr_fd;
        Unix.execv program (Array.of_list (program :: args))
      with _ -> Unix._exit 127)
    | pid ->
      child_pid := Some pid;
      Unix.close stdout_fd;
      Unix.close stderr_fd;
      let _, status = Unix.waitpid [] pid in
      child_pid := None;
      let stdout = read_file stdout_path in
      let stderr = read_file stderr_path in
      cleanup ();
      {status; stdout; stderr}
  with exn ->
    (try Unix.close stdout_fd with Unix.Unix_error _ -> ());
    (try Unix.close stderr_fd with Unix.Unix_error _ -> ());
    Option.iter
      (fun pid ->
        (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
        try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
      !child_pid;
    cleanup ();
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

let run_parallel ?(max_jobs = default_max_jobs) jobs =
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
    let signal_group signal (_, pid, _, _) =
      try Unix.kill (-pid) signal with Unix.Unix_error _ -> ()
    in
    List.iter (signal_group Sys.sigterm) children;
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
    let previous_mask =
      Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigterm]
    in
    let restore_signals () =
      ignore (Unix.sigprocmask Unix.SIG_SETMASK previous_mask)
    in
    let stdout_path = ref None in
    let stderr_path = ref None in
    let stdout_fd = ref None in
    let stderr_fd = ref None in
    let ready_read = ref None in
    let ready_write = ref None in
    try
      let stdout_log = temporary_log ~cwd:job.cwd "stdout" in
      stdout_path := Some stdout_log;
      let stderr_log = temporary_log ~cwd:job.cwd "stderr" in
      stderr_path := Some stderr_log;
      let out =
        Unix.openfile stdout_log [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      in
      stdout_fd := Some out;
      let err =
        Unix.openfile stderr_log [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      in
      stderr_fd := Some err;
      let read_end, write_end = Unix.pipe () in
      ready_read := Some read_end;
      ready_write := Some write_end;
      match Unix.fork () with
      | 0 -> (
        try
          Unix.close read_end;
          ignore (Unix.setsid ());
          ignore (Unix.write_substring write_end "1" 0 1);
          Unix.close write_end;
          restore_signals ();
          Unix.chdir job.cwd;
          Unix.dup2 out Unix.stdout;
          Unix.dup2 err Unix.stderr;
          Unix.close out;
          Unix.close err;
          Unix.execv job.program (Array.of_list (job.program :: job.args))
        with _ -> Unix._exit 127)
      | pid ->
        active := (index, pid, stdout_log, stderr_log) :: !active;
        Unix.close write_end;
        ready_write := None;
        let ready = Bytes.create 1 in
        ignore (Unix.read read_end ready 0 1);
        Unix.close read_end;
        ready_read := None;
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
      Option.iter
        (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        !ready_read;
      Option.iter
        (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        !ready_write;
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
          ignore (Unix.select [] [] [] 0.005);
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
