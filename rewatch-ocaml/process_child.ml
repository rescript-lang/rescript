type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}
type stdin_policy = Inherit_stdin | Null_stdin

exception Error of string

let decode_utf8_lossy value =
  if String.is_valid_utf_8 value then value
  else
    let output = Buffer.create (String.length value) in
    let rec loop index =
      if index < String.length value then (
        let decoded = String.get_utf_8_uchar value index in
        let length = max 1 (Uchar.utf_decode_length decoded) in
        if Uchar.utf_decode_is_valid decoded then
          Buffer.add_substring output value index length
        else Buffer.add_utf_8_uchar output Uchar.rep;
        loop (index + length))
    in
    loop 0;
    Buffer.contents output

let succeeded result = result.status = Unix.WEXITED 0

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

type capture = {
  thread: Thread.t;
  outcome: (string, exn) Stdlib.result option ref;
}

type child_wait = {
  thread: Thread.t;
  direct_outcome: (Unix.process_status, exn) Stdlib.result option Atomic.t;
  outcome: (result, exn) Stdlib.result option Atomic.t;
}

type 'a running = {
  payload: 'a;
  process: Platform.process;
  pid: int;
  child_wait: child_wait;
}

type launch_ownership = {
  mutable stdin: Unix.file_descr option;
  mutable stdout_read: Unix.file_descr option;
  mutable stdout_write: Unix.file_descr option;
  mutable stderr_read: Unix.file_descr option;
  mutable stderr_write: Unix.file_descr option;
  mutable stdout_capture: capture option;
  mutable stderr_capture: capture option;
  mutable process: Platform.process option;
  mutable child_wait: child_wait option;
  mutable termination_failed: bool;
}

let empty_launch_ownership () =
  {
    stdin = None;
    stdout_read = None;
    stdout_write = None;
    stderr_read = None;
    stderr_write = None;
    stdout_capture = None;
    stderr_capture = None;
    process = None;
    child_wait = None;
    termination_failed = false;
  }

type completion_notifier = {
  mutex: Mutex.t;
  condition: Condition.t;
  mutable generation: int;
  mutable stopped: bool;
}

let with_lock mutex action =
  Mutex.lock mutex;
  (* Unlocking in the exception path prevents one failed callback from
     permanently blocking every waiter that shares this notifier. *)
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) action

let create_completion_notifier () =
  {
    mutex = Mutex.create ();
    condition = Condition.create ();
    generation = 0;
    stopped = false;
  }

let notify_completion notifier =
  with_lock notifier.mutex (fun () ->
      if not notifier.stopped then (
        notifier.generation <- notifier.generation + 1;
        Condition.broadcast notifier.condition))

let notifier_generation notifier =
  with_lock notifier.mutex (fun () -> notifier.generation)

let await_notification notifier generation =
  with_lock notifier.mutex (fun () ->
      while notifier.generation = generation && not notifier.stopped do
        Condition.wait notifier.condition notifier.mutex
      done;
      notifier.generation)

let with_completion_notifier ~ticker_enabled action =
  (* The scheduler needs immediate child completion without repeatedly asking
     the operating system about every running PID. A condition variable wakes
     it when status and captured output are both ready; one ticker also wakes a
     supplied watch poll callback every five milliseconds while children are
     busy. *)
  let notifier = create_completion_notifier () in
  let rec send_tick () =
    Thread.delay 0.005;
    let continue =
      with_lock notifier.mutex (fun () ->
          if notifier.stopped then false
          else (
            notifier.generation <- notifier.generation + 1;
            Condition.broadcast notifier.condition;
            true))
    in
    if continue then send_tick ()
  in
  let deferred_signals = Signal_restore.create ~defer:true in
  let ticker = ref None in
  let stopped = ref false in
  let stop () =
    if not !stopped then (
      with_lock notifier.mutex (fun () ->
          notifier.stopped <- true;
          Condition.broadcast notifier.condition);
      Option.iter Thread.join !ticker;
      stopped := true)
  in
  try
    if ticker_enabled then ticker := Some (Thread.create send_tick ());
    Fun.protect ~finally:stop (fun () ->
        Signal_restore.restore deferred_signals;
        action notifier)
  with exn ->
    stop ();
    raise (Signal_restore.exception_after_restore deferred_signals exn)

let close_noerr descriptor =
  try Unix.close descriptor with Unix.Unix_error _ -> ()

let start_capture ?on_chunk descriptor : capture =
  let outcome = ref None in
  let thread =
    Thread.create
      (fun () ->
        outcome :=
          Some
            (try
               Fun.protect
                 ~finally:(fun () -> close_noerr descriptor)
                 (fun () ->
                   let output = Buffer.create 4096 in
                   let bytes = Bytes.create 65536 in
                   let rec read () =
                     try
                       match
                         Unix.read descriptor bytes 0 (Bytes.length bytes)
                       with
                       | 0 -> ()
                       | count ->
                         (match on_chunk with
                         | Some on_chunk -> on_chunk bytes count
                         | None -> Buffer.add_subbytes output bytes 0 count);
                         read ()
                     with Unix.Unix_error (Unix.EINTR, _, _) -> read ()
                   in
                   read ();
                   Ok
                     (match on_chunk with
                     | Some _ -> ""
                     | None -> Buffer.contents output |> decode_utf8_lossy))
             with exn -> Error exn))
      ()
  in
  {thread; outcome}

let capture_outcome (capture : capture) =
  Thread.join capture.thread;
  match !(capture.outcome) with
  | Some outcome -> outcome
  | None -> Error (Failure "subprocess output reader did not finish")

let capture_error exn =
  Error ("failed to capture subprocess output: " ^ Printexc.to_string exn)

let start_child_wait pid notifier stdout_capture stderr_capture : child_wait =
  let direct_outcome = Atomic.make None in
  let outcome = Atomic.make None in
  let rec wait () =
    try
      let _, status = Unix.waitpid [] pid in
      status
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  let thread =
    Thread.create
      (fun () ->
        let status = try Ok (wait ()) with exn -> Error exn in
        Atomic.set direct_outcome (Some status);
        let stdout = capture_outcome stdout_capture in
        let stderr = capture_outcome stderr_capture in
        let result =
          match (status, stdout, stderr) with
          | Ok status, Ok stdout, Ok stderr -> Ok {status; stdout; stderr}
          | Error exn, _, _ -> Error exn
          | _, Error exn, _ | _, _, Error exn -> Error (capture_error exn)
        in
        Atomic.set outcome (Some result);
        notify_completion notifier)
      ()
  in
  {thread; direct_outcome; outcome}

let fail_launch ownership deferred_signals launch_error =
  Option.iter
    (fun process ->
      let pid = Platform.process_id process in
      let root_reaped =
        match ownership.child_wait with
        | Some wait -> Option.is_some (Atomic.get wait.direct_outcome)
        | None -> false
      in
      if not (Platform.signal_process_tree ~root_reaped process Sys.sigkill)
      then ownership.termination_failed <- true;
      if
        (not ownership.termination_failed)
        && Option.is_none ownership.child_wait
      then try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
    ownership.process;
  List.iter
    (fun descriptor -> Option.iter close_noerr descriptor)
    [
      ownership.stdout_write;
      ownership.stderr_write;
      ownership.stdout_read;
      ownership.stderr_read;
      ownership.stdin;
    ];
  (match ownership.child_wait with
  | Some wait when not ownership.termination_failed -> Thread.join wait.thread
  | Some _ -> ()
  | None
    when (not ownership.termination_failed) && Option.is_some ownership.process
    ->
    Option.iter
      (fun (capture : capture) -> Thread.join capture.thread)
      ownership.stdout_capture;
    Option.iter
      (fun (capture : capture) -> Thread.join capture.thread)
      ownership.stderr_capture
  | None -> ());
  let release_error =
    try
      Option.iter Platform.release_process ownership.process;
      None
    with release_exn -> Some release_exn
  in
  let restore_error =
    try
      Signal_restore.restore deferred_signals;
      None
    with signal_exn -> Some signal_exn
  in
  let error =
    if ownership.termination_failed then
      Error "Could not terminate a partially launched subprocess tree"
    else
      match (restore_error, release_error) with
      | Some signal_exn, _ -> signal_exn
      | None, Some release_exn -> release_exn
      | None, None -> launch_error
  in
  raise error

let launch ?env ?stdout_chunk ?stderr_chunk ?(stdin = Null_stdin)
    ?(defer_signals = true) ~notifier payload job =
  (* Capture descriptors need a cleanup owner before asynchronous watch
     termination can raise. Signals are therefore deferred across pipe
     acquisition and restored only after every descriptor has an owner. *)
  let deferred_signals = Signal_restore.create ~defer:defer_signals in
  let ownership = empty_launch_ownership () in
  try
    let pipes = Platform.create_capture_pipes () in
    let (stdout_read, stdout_write), (stderr_read, stderr_write) = pipes in
    ownership.stdout_read <- Some stdout_read;
    ownership.stdout_write <- Some stdout_write;
    ownership.stderr_read <- Some stderr_read;
    ownership.stderr_write <- Some stderr_write;
    let stdout = start_capture ?on_chunk:stdout_chunk stdout_read in
    ownership.stdout_read <- None;
    ownership.stdout_capture <- Some stdout;
    let stderr = start_capture ?on_chunk:stderr_chunk stderr_read in
    ownership.stderr_read <- None;
    ownership.stderr_capture <- Some stderr;
    let stdin =
      match stdin with
      | Inherit_stdin -> Unix.stdin
      | Null_stdin ->
        let descriptor =
          Unix.openfile Platform.null_device [Unix.O_RDONLY; Unix.O_CLOEXEC] 0
        in
        ownership.stdin <- Some descriptor;
        descriptor
    in
    let process =
      Platform.spawn ~env ~cwd:job.cwd ~program:job.program ~args:job.args
        ~stdin ~stdout:stdout_write ~stderr:stderr_write
    in
    ownership.process <- Some process;
    let pid = Platform.process_id process in
    Option.iter close_noerr ownership.stdin;
    ownership.stdin <- None;
    ownership.stdout_write <- None;
    close_noerr stdout_write;
    ownership.stderr_write <- None;
    close_noerr stderr_write;
    let wait = start_child_wait pid notifier stdout stderr in
    ownership.child_wait <- Some wait;
    Signal_restore.restore deferred_signals;
    {payload; process; pid; child_wait = wait}
  with exn -> fail_launch ownership deferred_signals exn

let wait_for_running ~poll ?(defer_signals = true) notifier active =
  let rec find_completed = function
    | [] -> None
    | (child : _ running) :: rest -> (
      match Atomic.get child.child_wait.outcome with
      | Some outcome -> Some (child, outcome)
      | None -> find_completed rest)
  in
  let rec wait generation =
    match find_completed active with
    | Some (child, Ok result) ->
      let deferred_signals = Signal_restore.create ~defer:defer_signals in
      ((child, result), deferred_signals)
    | Some (_, Error exn) -> raise exn
    | None ->
      poll ();
      await_notification notifier generation |> wait
  in
  notifier_generation notifier |> wait

let signal_running (children : _ running list) =
  if children <> [] then (
    let root_identity_lost (child : _ running) =
      Option.is_some (Atomic.get child.child_wait.direct_outcome)
    in
    let signal_group signal (child : _ running) =
      let root_reaped = root_identity_lost child in
      Platform.signal_process_tree ~root_reaped child.process signal
    in
    let signal_all signal =
      List.fold_left
        (fun succeeded child -> signal_group signal child && succeeded)
        true children
    in
    let graceful_signal = Platform.graceful_termination_signal in
    let graceful_succeeded = signal_all graceful_signal in
    let deadline = Unix.gettimeofday () +. 0.25 in
    let rec wait_until_deadline children =
      let remaining =
        List.filter (fun child -> not (root_identity_lost child)) children
      in
      if remaining <> [] && Unix.gettimeofday () < deadline then (
        ignore (Unix.select [] [] [] 0.01);
        wait_until_deadline remaining)
      else remaining
    in
    ignore (wait_until_deadline children);
    (* Every original process group needs escalation because a direct child can
       exit while a PPX or helper in its group remains alive. *)
    let escalation_succeeded =
      if Platform.escalate_process_groups then signal_all Sys.sigkill else true
    in
    if not (graceful_succeeded && escalation_succeeded) then
      raise (Error "Could not terminate a subprocess tree"))

let release_after_completion (child : _ running) =
  ignore
    (Thread.create
       (fun () ->
         Thread.join child.child_wait.thread;
         Platform.release_process child.process)
       ())

let payload (child : _ running) = child.payload
let pid (child : _ running) = child.pid
let await_termination (child : _ running) = Thread.join child.child_wait.thread
let release (child : _ running) = Platform.release_process child.process

let terminate_running (children : _ running list) =
  if children <> [] then (
    try
      signal_running children;
      List.iter
        (fun (child : _ running) ->
          Thread.join child.child_wait.thread;
          Platform.release_process child.process)
        children
    with exn ->
      List.iter release_after_completion children;
      raise exn)

let release_running (child : _ running) =
  await_termination child;
  release child
