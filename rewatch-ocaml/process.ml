type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}
type task = {job: job; env: Spawn.Env.t option; on_result: result -> result}

exception Error of string
exception Interrupted of int

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

(* Reader threads are needed because a child can block when either output pipe
   fills, and Windows cannot use select to drain anonymous pipes concurrently.
   The threads perform only blocking I/O; dependency scheduling stays on the
   calling thread. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

let task ?env ?(on_result = fun result -> result) job = {job; env; on_result}

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

type completion_notifier = {
  mutex: Mutex.t;
  condition: Condition.t;
  mutable generation: int;
  mutable stopped: bool;
}

let with_mutex mutex action =
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
  with_mutex notifier.mutex (fun () ->
      if not notifier.stopped then (
        notifier.generation <- notifier.generation + 1;
        Condition.broadcast notifier.condition))

let notifier_generation notifier =
  with_mutex notifier.mutex (fun () -> notifier.generation)

let await_notification notifier generation =
  with_mutex notifier.mutex (fun () ->
      while notifier.generation = generation && not notifier.stopped do
        Condition.wait notifier.condition notifier.mutex
      done;
      notifier.generation)

let with_completion_notifier ?(ticker_enabled = false) action =
  (* The scheduler needs immediate child completion without repeatedly asking
     the operating system about every running PID. A condition variable wakes
     it when status and captured output are both ready; one ticker also wakes a
     supplied watch poll callback every five milliseconds while children are
     busy. *)
  let notifier = create_completion_notifier () in
  let rec send_tick () =
    Thread.delay 0.005;
    let continue =
      with_mutex notifier.mutex (fun () ->
          if notifier.stopped then false
          else (
            notifier.generation <- notifier.generation + 1;
            Condition.broadcast notifier.condition;
            true))
    in
    if continue then send_tick ()
  in
  let restore_signals = Platform.defer_termination_signals () in
  let ticker = ref None in
  let stopped = ref false in
  let stop () =
    if not !stopped then (
      with_mutex notifier.mutex (fun () ->
          notifier.stopped <- true;
          Condition.broadcast notifier.condition);
      Option.iter Thread.join !ticker;
      stopped := true)
  in
  try
    if ticker_enabled then ticker := Some (Thread.create send_tick ());
    Fun.protect ~finally:stop (fun () ->
        restore_signals ();
        action notifier)
  with exn ->
    stop ();
    let exn =
      try
        restore_signals ();
        exn
      with signal_exn -> signal_exn
    in
    raise exn

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
               let output = Buffer.create 4096 in
               let bytes = Bytes.create 65536 in
               let rec read () =
                 try
                   match Unix.read descriptor bytes 0 (Bytes.length bytes) with
                   | 0 -> ()
                   | count ->
                     (match on_chunk with
                     | Some on_chunk -> on_chunk bytes count
                     | None -> Buffer.add_subbytes output bytes 0 count);
                     read ()
                 with Unix.Unix_error (Unix.EINTR, _, _) -> read ()
               in
               Fun.protect ~finally:(fun () -> close_noerr descriptor) read;
               Ok
                 (match on_chunk with
                 | Some _ -> ""
                 | None -> Buffer.contents output |> decode_utf8_lossy)
             with exn ->
               close_noerr descriptor;
               Error exn))
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

let launch ?env ?stdout_chunk ?stderr_chunk ?(defer_signals = true) ~notifier
    payload job =
  (* Capture descriptors need a cleanup owner before asynchronous watch
     termination can raise. Signals are therefore deferred across pipe
     acquisition and restored only after every descriptor has an owner. *)
  let restore_signals =
    if defer_signals then Platform.defer_termination_signals () else Fun.id
  in
  let opened_pipes = ref None in
  let stdout_capture = ref None in
  let stderr_capture = ref None in
  let child_process = ref None in
  let child_wait = ref None in
  let termination_failed = ref false in
  try
    let pipes = Platform.create_capture_pipes () in
    opened_pipes := Some pipes;
    let (stdout_read, stdout_write), (stderr_read, stderr_write) = pipes in
    let stdout = start_capture ?on_chunk:stdout_chunk stdout_read in
    stdout_capture := Some stdout;
    let stderr = start_capture ?on_chunk:stderr_chunk stderr_read in
    stderr_capture := Some stderr;
    let process =
      Platform.spawn ~env ~cwd:job.cwd ~program:job.program ~args:job.args
        ~stdout:stdout_write ~stderr:stderr_write
    in
    child_process := Some process;
    let pid = Platform.process_id process in
    close_noerr stdout_write;
    close_noerr stderr_write;
    let wait = start_child_wait pid notifier stdout stderr in
    child_wait := Some wait;
    restore_signals ();
    {payload; process; pid; child_wait = wait}
  with exn ->
    Option.iter
      (fun ((stdout_read, stdout_write), (stderr_read, stderr_write)) ->
        Option.iter
          (fun process ->
            let pid = Platform.process_id process in
            let root_reaped =
              match !child_wait with
              | Some wait -> Option.is_some (Atomic.get wait.direct_outcome)
              | None -> false
            in
            if
              not
                (Platform.signal_process_tree ~root_reaped process Sys.sigkill)
            then termination_failed := true;
            if (not !termination_failed) && Option.is_none !child_wait then
              try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
          !child_process;
        close_noerr stdout_write;
        close_noerr stderr_write;
        if Option.is_none !stdout_capture then close_noerr stdout_read;
        if Option.is_none !stderr_capture then close_noerr stderr_read;
        match !child_wait with
        | Some wait when not !termination_failed -> Thread.join wait.thread
        | Some _ -> ()
        | None when (not !termination_failed) && Option.is_some !child_process
          ->
          Option.iter
            (fun (capture : capture) -> Thread.join capture.thread)
            !stdout_capture;
          Option.iter
            (fun (capture : capture) -> Thread.join capture.thread)
            !stderr_capture
        | None -> ())
      !opened_pipes;
    let release_error =
      try
        Option.iter Platform.release_process !child_process;
        None
      with release_exn -> Some release_exn
    in
    let restore_error =
      try
        restore_signals ();
        None
      with signal_exn -> Some signal_exn
    in
    let exn =
      if !termination_failed then
        Error "Could not terminate a partially launched subprocess tree"
      else
        match (restore_error, release_error) with
        | Some signal_exn, _ -> signal_exn
        | None, Some release_exn -> release_exn
        | None, None -> exn
    in
    raise exn

let wait_for_running ?(poll = fun () -> ()) ?(defer_signals = true) notifier
    active =
  let rec find_completed = function
    | [] -> None
    | child :: rest -> (
      match Atomic.get child.child_wait.outcome with
      | Some outcome -> Some (child, outcome)
      | None -> find_completed rest)
  in
  let rec wait generation =
    match find_completed active with
    | Some (child, Ok result) ->
      let restore_signals =
        if defer_signals then Platform.defer_termination_signals () else Fun.id
      in
      ((child, result), restore_signals)
    | Some (_, Error exn) -> raise exn
    | None ->
      poll ();
      await_notification notifier generation |> wait
  in
  notifier_generation notifier |> wait

let with_signal_restore restore_signals action =
  try
    let result = action () in
    restore_signals ();
    result
  with exn ->
    let exn =
      try
        restore_signals ();
        exn
      with signal_exn -> signal_exn
    in
    raise exn

let signal_running children =
  if children <> [] then (
    let root_identity_lost child =
      Option.is_some (Atomic.get child.child_wait.direct_outcome)
    in
    let signal_group signal child =
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

let terminate_running children =
  if children <> [] then (
    try
      signal_running children;
      List.iter
        (fun child ->
          Thread.join child.child_wait.thread;
          Platform.release_process child.process)
        children
    with exn ->
      List.iter (fun child -> Platform.release_process child.process) children;
      raise exn)

let release_running child =
  Thread.join child.child_wait.thread;
  Platform.release_process child.process

let run_parallel_map_with_notifier ~max_jobs ~poll ~on_complete notifier values
    ~job =
  let indexed = List.mapi (fun index value -> (index, value)) values in
  let results = Array.make (List.length values) None in
  let active = ref [] in
  let launch_indexed (index, value) =
    active := launch ~notifier index (job value) :: !active
  in
  let rec fill slots queued =
    if slots = 0 then queued
    else
      match queued with
      | [] -> []
      | job :: rest ->
        launch_indexed job;
        fill (slots - 1) rest
  in
  let rec schedule queued =
    let queued = fill (max_jobs - List.length !active) queued in
    match !active with
    | [] -> ()
    | _ ->
      let (child, result), restore_signals =
        wait_for_running ~poll notifier !active
      in
      with_signal_restore restore_signals (fun () ->
          active :=
            List.filter (fun running -> running.pid <> child.pid) !active;
          release_running child;
          results.(child.payload) <- Some result;
          on_complete child.payload);
      schedule queued
  in
  try
    schedule indexed;
    Array.to_list results
    |> List.map (function
      | Some result -> result
      | None -> raise (Error "subprocess result was not collected"))
  with exn ->
    terminate_running !active;
    raise exn

let run_parallel_map ?(max_jobs = default_max_jobs) ?poll
    ?(on_complete = fun _ -> ()) values ~job =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  match values with
  | [] -> []
  | _ ->
    let poll, ticker_enabled =
      match poll with
      | Some poll -> (poll, true)
      | None -> ((fun () -> ()), false)
    in
    with_completion_notifier ~ticker_enabled (fun notifier ->
        run_parallel_map_with_notifier ~max_jobs ~poll ~on_complete notifier
          values ~job)

let run_parallel ?max_jobs ?poll ?on_complete jobs =
  run_parallel_map ?max_jobs ?poll ?on_complete jobs ~job:Fun.id

type 'a work = {key: string; dependencies: string list; value: 'a}

module Work_ready = Set.Make (struct
  type t = int * string

  let compare (first_priority, first_key) (second_priority, second_key) =
    let by_priority = compare second_priority first_priority in
    if by_priority <> 0 then by_priority
    else String.compare first_key second_key
end)

type pool_active = {
  id: int;
  child: unit running;
  mutable cancellation_requested: bool;
}

type 'a pool_completion =
  | Task_completed of 'a * result
  | Task_failed of 'a * exn

type 'a worker_pool = {
  notifier: completion_notifier;
  mutex: Mutex.t;
  work_available: Condition.t;
  cancellation_finished: Condition.t;
  queued: ('a * task) Queue.t;
  completed_tasks: 'a pool_completion Queue.t;
  mutable active_children: pool_active list;
  mutable next_active_id: int;
  mutable stopping: bool;
  mutable signalling_cancellation: bool;
  mutable workers: unit Domain.t list;
}

let launch_worker_task notifier task =
  (* Signal handlers are process-wide and may execute on a worker domain. A
     worker therefore keeps ownership across launch without temporarily
     replacing those handlers: an interruption unwinds through [launch] or the
     completion queue, and the scheduler then cancels the other process trees. *)
  launch ?env:task.env ~defer_signals:false ~notifier () task.job

let remove_active pool active =
  with_mutex pool.mutex (fun () ->
      while pool.signalling_cancellation && active.cancellation_requested do
        Condition.wait pool.cancellation_finished pool.mutex
      done;
      pool.active_children <-
        List.filter
          (fun current -> current.id <> active.id)
          pool.active_children);
  Platform.release_process active.child.process

let complete_pool_task pool completion =
  with_mutex pool.mutex (fun () -> Queue.add completion pool.completed_tasks);
  notify_completion pool.notifier

let run_pool_task pool payload task =
  match
    try Ok (launch_worker_task pool.notifier task) with exn -> Error exn
  with
  | Error exn -> complete_pool_task pool (Task_failed (payload, exn))
  | Ok child ->
    let active, cancel_after_launch =
      with_mutex pool.mutex (fun () ->
          let active =
            {
              id = pool.next_active_id;
              child;
              cancellation_requested = pool.stopping;
            }
          in
          pool.next_active_id <- pool.next_active_id + 1;
          pool.active_children <- active :: pool.active_children;
          (active, active.cancellation_requested))
    in
    let task_completion =
      try
        if cancel_after_launch then signal_running [child];
        let (_, result), restore_signals =
          wait_for_running ~defer_signals:false pool.notifier [child]
        in
        with_signal_restore restore_signals (fun () ->
            try Task_completed (payload, task.on_result result)
            with exn -> Task_failed (payload, exn))
      with exn -> Task_failed (payload, exn)
    in
    let completion =
      try
        Thread.join child.child_wait.thread;
        remove_active pool active;
        task_completion
      with exn -> Task_failed (payload, exn)
    in
    complete_pool_task pool completion

let rec worker_loop pool =
  let queued =
    with_mutex pool.mutex (fun () ->
        while Queue.is_empty pool.queued && not pool.stopping do
          Condition.wait pool.work_available pool.mutex
        done;
        if Queue.is_empty pool.queued then None
        else Some (Queue.take pool.queued))
  in
  match queued with
  | None -> ()
  | Some (payload, task) ->
    run_pool_task pool payload task;
    worker_loop pool

let create_worker_pool ~max_jobs notifier =
  let pool =
    {
      notifier;
      mutex = Mutex.create ();
      work_available = Condition.create ();
      cancellation_finished = Condition.create ();
      queued = Queue.create ();
      completed_tasks = Queue.create ();
      active_children = [];
      next_active_id = 0;
      stopping = false;
      signalling_cancellation = false;
      workers = [];
    }
  in
  let rec start_workers remaining =
    if remaining > 0 then (
      let worker = Domain.spawn (fun () -> worker_loop pool) in
      pool.workers <- worker :: pool.workers;
      start_workers (remaining - 1))
  in
  try
    start_workers max_jobs;
    pool
  with exn ->
    with_mutex pool.mutex (fun () ->
        pool.stopping <- true;
        Condition.broadcast pool.work_available);
    List.iter Domain.join pool.workers;
    raise exn

let submit_pool_task pool payload task =
  with_mutex pool.mutex (fun () ->
      if pool.stopping then raise (Error "subprocess worker pool is stopping");
      Queue.add (payload, task) pool.queued;
      Condition.signal pool.work_available)

let await_pool_completion ~poll pool =
  let rec wait generation =
    match
      with_mutex pool.mutex (fun () ->
          if Queue.is_empty pool.completed_tasks then None
          else Some (Queue.take pool.completed_tasks))
    with
    | Some completion -> completion
    | None ->
      poll ();
      await_notification pool.notifier generation |> wait
  in
  notifier_generation pool.notifier |> wait

let stop_worker_pool ~cancel pool =
  let active =
    with_mutex pool.mutex (fun () ->
        pool.stopping <- true;
        Queue.clear pool.queued;
        Condition.broadcast pool.work_available;
        if cancel then (
          pool.signalling_cancellation <- true;
          pool.active_children
          |> List.filter_map (fun active ->
              if active.cancellation_requested then None
              else (
                active.cancellation_requested <- true;
                Some active.child)))
        else [])
  in
  let signal_error =
    try
      signal_running active;
      None
    with exn -> Some exn
  in
  with_mutex pool.mutex (fun () ->
      pool.signalling_cancellation <- false;
      Condition.broadcast pool.cancellation_finished);
  List.iter Domain.join pool.workers;
  Option.iter raise signal_error

let with_worker_pool ~max_jobs notifier action =
  let pool = create_worker_pool ~max_jobs notifier in
  match action pool with
  | result ->
    stop_worker_pool ~cancel:false pool;
    result
  | exception exn ->
    let exn =
      try
        stop_worker_pool ~cancel:true pool;
        exn
      with cancellation_exn -> cancellation_exn
    in
    raise exn

let run_dependency_graph_with_notifier ~max_jobs ~is_fatal ~poll notifier works
    ~next =
  let count = List.length works in
  let by_key = Hashtbl.create count in
  List.iter
    (fun work ->
      if Hashtbl.mem by_key work.key then
        raise (Error ("duplicate subprocess work key: " ^ work.key));
      Hashtbl.add by_key work.key work)
    works;
  let dependents = Hashtbl.create count in
  let dependencies_by_key = Hashtbl.create count in
  let pending = Hashtbl.create count in
  List.iter
    (fun work ->
      let dependencies = List.sort_uniq String.compare work.dependencies in
      Hashtbl.add dependencies_by_key work.key dependencies;
      List.iter
        (fun dependency ->
          if not (Hashtbl.mem by_key dependency) then
            raise
              (Error
                 (Printf.sprintf "unknown dependency %s for subprocess work %s"
                    dependency work.key));
          let current =
            Hashtbl.find_opt dependents dependency |> Option.value ~default:[]
          in
          Hashtbl.replace dependents dependency (work :: current))
        dependencies;
      Hashtbl.add pending work.key (List.length dependencies))
    works;
  let priorities = Hashtbl.create count in
  let remaining_dependents = Hashtbl.create count in
  let leaves = Queue.create () in
  List.iter
    (fun work ->
      let dependent_count =
        Hashtbl.find_opt dependents work.key
        |> Option.value ~default:[] |> List.length
      in
      Hashtbl.add remaining_dependents work.key dependent_count;
      if dependent_count = 0 then (
        Hashtbl.add priorities work.key 1;
        Queue.add work.key leaves))
    works;
  let prioritized = ref 0 in
  while not (Queue.is_empty leaves) do
    let key = Queue.take leaves in
    incr prioritized;
    let key_priority = Hashtbl.find priorities key in
    Hashtbl.find dependencies_by_key key
    |> List.iter (fun dependency ->
        let candidate = key_priority + 1 in
        let current =
          Hashtbl.find_opt priorities dependency |> Option.value ~default:1
        in
        if candidate > current then
          Hashtbl.replace priorities dependency candidate;
        let remaining = Hashtbl.find remaining_dependents dependency - 1 in
        Hashtbl.replace remaining_dependents dependency remaining;
        if remaining = 0 then Queue.add dependency leaves)
  done;
  let () =
    if !prioritized <> count then
      let cycle =
        Graph.shortest_cycle works
          ~name:(fun work -> work.key)
          ~deps:(fun work -> work.dependencies)
        |> Option.value ~default:[]
      in
      let details =
        match cycle with
        | [] -> ""
        | cycle -> ": " ^ String.concat " -> " cycle
      in
      raise (Error ("subprocess dependency graph contains a cycle" ^ details))
  in
  let ready = ref Work_ready.empty in
  let add_ready work =
    ready := Work_ready.add (Hashtbl.find priorities work.key, work.key) !ready
  in
  List.iter
    (fun work -> if Hashtbl.find pending work.key = 0 then add_ready work)
    works;
  let in_flight = ref 0 in
  let completed = ref 0 in
  let stopped = ref false in
  let errors = ref [] in
  let record_error work exn =
    if is_fatal exn then raise exn
    else (
      stopped := true;
      errors := (work.key, exn) :: !errors)
  in
  let complete work =
    incr completed;
    Hashtbl.find_opt dependents work.key
    |> Option.value ~default:[]
    |> List.iter (fun dependent ->
        let remaining = Hashtbl.find pending dependent.key - 1 in
        Hashtbl.replace pending dependent.key remaining;
        if remaining = 0 then add_ready dependent)
  in
  let rec fill pool =
    if (not !stopped) && !in_flight < max_jobs then
      match Work_ready.min_elt_opt !ready with
      | None -> ()
      | Some ((_, key) as ready_key) ->
        ready := Work_ready.remove ready_key !ready;
        let work = Hashtbl.find by_key key in
        (try
           match next work.value None with
           | None -> complete work
           | Some task ->
             submit_pool_task pool work task;
             incr in_flight
         with exn -> record_error work exn);
        fill pool
  in
  let rec schedule pool =
    fill pool;
    if !in_flight = 0 then
      match
        !errors
        |> List.sort (fun (first, _) (second, _) -> String.compare first second)
      with
      | (_, exn) :: _ -> raise exn
      | [] when !completed <> count ->
        raise (Error "subprocess dependency graph stalled")
      | [] -> ()
    else
      let completion = await_pool_completion ~poll pool in
      decr in_flight;
      (match completion with
      | Task_failed (work, exn) -> record_error work exn
      | Task_completed (work, result) -> (
        try
          match next work.value (Some result) with
          | Some task ->
            submit_pool_task pool work task;
            incr in_flight
          | None -> complete work
        with exn -> record_error work exn));
      schedule pool
  in
  with_worker_pool ~max_jobs:(min max_jobs count) notifier schedule

let run_dependency_graph ?(max_jobs = default_max_jobs)
    ?(is_fatal =
      function
      | Sys.Break | Interrupted _ -> true
      | _ -> false) ?poll works ~next =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  match works with
  | [] -> ()
  | _ ->
    let poll, ticker_enabled =
      match poll with
      | Some poll -> (poll, true)
      | None -> ((fun () -> ()), false)
    in
    with_completion_notifier ~ticker_enabled (fun notifier ->
        run_dependency_graph_with_notifier ~max_jobs ~is_fatal ~poll notifier
          works ~next)

let run_one ?env ?poll ?stdout_chunk ?stderr_chunk ~cwd program args =
  let poll, ticker_enabled =
    match poll with
    | Some poll -> (poll, true)
    | None -> ((fun () -> ()), false)
  in
  with_completion_notifier ~ticker_enabled (fun notifier ->
      let child =
        launch ?env ?stdout_chunk ?stderr_chunk ~notifier ()
          {program; args; cwd}
      in
      let reaped = ref false in
      try
        let (_, result), restore_signals =
          wait_for_running ~poll notifier [child]
        in
        reaped := true;
        with_signal_restore restore_signals (fun () ->
            release_running child;
            result)
      with exn ->
        if not !reaped then terminate_running [child];
        raise exn)

let run ?env ?poll ~cwd program args = run_one ?env ?poll ~cwd program args

let run_streaming ?env ?poll ~cwd program args =
  let write channel bytes count =
    output channel bytes 0 count;
    flush channel
  in
  run_one ?env ?poll ~stdout_chunk:(write stdout) ~stderr_chunk:(write stderr)
    ~cwd program args
