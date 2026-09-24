module Child = Process_child

type result = Child.result = {
  status: Unix.process_status;
  stdout: string;
  stderr: string;
}

type job = Child.job = {program: string; args: string list; cwd: string}
type task_kind =
  | External of {job: job; env: Spawn.Env.t option}
  | In_process of (unit -> result)
  | Concurrent of {run: unit -> result; cancel: unit -> unit}

type task = {kind: task_kind; on_result: result -> result}

exception Error = Child.Error
exception Interrupted of int

let decode_utf8_lossy = Child.decode_utf8_lossy
let succeeded = Child.succeeded
let status_string = Child.status_string

(* Reader threads are needed because a child can block when either output pipe
   fills, and Windows cannot use select to drain anonymous pipes concurrently.
   The threads perform only blocking I/O; dependency scheduling stays on the
   calling thread. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

let task ?env ?(on_result = fun result -> result) job =
  {kind = External {job; env}; on_result}

let in_process_task ?(on_result = fun result -> result) run =
  {kind = In_process run; on_result}

let concurrent_task ~cancel ?(on_result = fun result -> result) run =
  {kind = Concurrent {run; cancel}; on_result}

let map_result task map =
  {task with on_result = (fun result -> map (task.on_result result))}

let run_parallel_map_with_notifier ~max_jobs ~poll ~on_complete notifier values
    ~job =
  let indexed = List.mapi (fun index value -> (index, value)) values in
  let results = Array.make (List.length values) None in
  let active = ref [] in
  let launch_indexed (index, value) =
    active := Child.launch ~notifier index (job value) :: !active
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
      let (child, result), deferred_signals =
        Child.wait_for_running ~poll notifier !active
      in
      Signal_restore.protect deferred_signals (fun () ->
          active := List.filter (fun running -> running != child) !active;
          Child.release_running child;
          results.(Child.payload child) <- Some result;
          on_complete (Child.payload child));
      schedule queued
  in
  try
    schedule indexed;
    Array.to_list results
    |> List.map (function
      | Some result -> result
      | None -> raise (Error "subprocess result was not collected"))
  with exn ->
    Child.terminate_running !active;
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
    Child.with_completion_notifier ~ticker_enabled (fun notifier ->
        run_parallel_map_with_notifier ~max_jobs ~poll ~on_complete notifier
          values ~job)

let run_parallel ?max_jobs ?poll ?on_complete jobs =
  run_parallel_map ?max_jobs ?poll ?on_complete jobs ~job:Fun.id

type 'a work = {key: string; dependencies: string list; value: 'a}
type failure_action =
  | Abort_immediately
  | Stop_new_work
  | Continue_independent_work

module Work_ready = Set.Make (struct
  type t = int * string

  let compare (first_priority, first_key) (second_priority, second_key) =
    let by_priority = compare second_priority first_priority in
    if by_priority <> 0 then by_priority
    else String.compare first_key second_key
end)

type cancellation_state =
  | Cancellation_not_requested
  | Cancellation_requested
  | Cancellation_failed of exn

type termination_state =
  | Termination_confirmed
  | Termination_unconfirmed of exn

type pool_active = {
  id: int;
  child: unit Child.running;
  mutable cancellation: cancellation_state;
}

type pool_action = {id: int; cancel: unit -> unit}

let cancellation_was_requested = function
  | Cancellation_not_requested -> false
  | Cancellation_requested | Cancellation_failed _ -> true

type 'a pool_completion =
  | Task_completed of 'a * result
  | Task_failed of 'a * exn

type 'a worker_pool = {
  notifier: Child.completion_notifier;
  mutex: Mutex.t;
  work_available: Condition.t;
  cancellation_finished: Condition.t;
  queued: ('a * task) Queue.t;
  completed_tasks: 'a pool_completion Queue.t;
  mutable active_children: pool_active list;
  mutable active_actions: pool_action list;
  mutable next_active_id: int;
  mutable stopping: bool;
  mutable signalling_cancellation: bool;
  mutable workers: unit Domain.t list;
}

let launch_worker_task notifier job env =
  (* Signal handlers are process-wide and may execute on a worker domain. A
     worker therefore keeps ownership across launch without temporarily
     replacing those handlers: an interruption unwinds through [launch] or the
     completion queue, and the scheduler then cancels the other process trees. *)
  Child.launch ?env ~defer_signals:false ~notifier () job

let remove_active pool (active : pool_active) =
  Child.with_lock pool.mutex (fun () ->
      while
        pool.signalling_cancellation
        && cancellation_was_requested active.cancellation
      do
        Condition.wait pool.cancellation_finished pool.mutex
      done;
      pool.active_children <-
        List.filter
          (fun (current : pool_active) -> current.id <> active.id)
          pool.active_children)

let release_active active = Child.release active.child

let release_active_after_completion active =
  Child.release_after_completion active.child

let complete_pool_task pool completion =
  Child.with_lock pool.mutex (fun () ->
      Queue.add completion pool.completed_tasks);
  Child.notify_completion pool.notifier

let run_concurrent_task pool payload task run cancel =
  let active, cancel_now =
    Child.with_lock pool.mutex (fun () ->
        let active = {id = pool.next_active_id; cancel} in
        pool.next_active_id <- pool.next_active_id + 1;
        pool.active_actions <- active :: pool.active_actions;
        (active, pool.stopping))
  in
  let completion =
    try
      if cancel_now then cancel ();
      Task_completed (payload, task.on_result (run ()))
    with exn -> Task_failed (payload, exn)
  in
  Child.with_lock pool.mutex (fun () ->
      pool.active_actions <-
        List.filter (fun current -> current.id <> active.id) pool.active_actions);
  complete_pool_task pool completion

let run_pool_task pool payload task =
  match task.kind with
  | Concurrent {run; cancel} -> run_concurrent_task pool payload task run cancel
  | In_process run ->
    let completion =
      try Task_completed (payload, task.on_result (run ()))
      with exn -> Task_failed (payload, exn)
    in
    complete_pool_task pool completion
  | External {job; env} -> (
    match
      try Ok (launch_worker_task pool.notifier job env) with exn -> Error exn
    with
    | Error exn -> complete_pool_task pool (Task_failed (payload, exn))
    | Ok child ->
      let active, cancel_after_launch =
        Child.with_lock pool.mutex (fun () ->
            let active =
              {
                id = pool.next_active_id;
                child;
                cancellation =
                  (if pool.stopping then Cancellation_requested
                   else Cancellation_not_requested);
              }
            in
            pool.next_active_id <- pool.next_active_id + 1;
            pool.active_children <- active :: pool.active_children;
            (active, cancellation_was_requested active.cancellation))
      in
      let wait_result =
        try
          if cancel_after_launch then Child.signal_running [child];
          Ok
            (Child.wait_for_running ~defer_signals:false
               ~poll:(fun () ->
                 match
                   Child.with_lock pool.mutex (fun () -> active.cancellation)
                 with
                 | Cancellation_failed exn -> raise exn
                 | Cancellation_not_requested | Cancellation_requested -> ())
               pool.notifier [child])
        with exn -> Error exn
      in
      let completion =
        match wait_result with
        | Ok ((_, result), deferred_signals) -> (
          remove_active pool active;
          try
            Signal_restore.protect deferred_signals (fun () ->
                Child.await_termination child;
                release_active active;
                Task_completed (payload, task.on_result result))
          with exn -> Task_failed (payload, exn))
        | Error exn -> (
          let termination =
            try
              Child.signal_running [child];
              Termination_confirmed
            with cancellation_exn -> Termination_unconfirmed cancellation_exn
          in
          match termination with
          | Termination_confirmed -> (
            remove_active pool active;
            try
              Child.await_termination child;
              release_active active;
              Task_failed (payload, exn)
            with release_exn -> Task_failed (payload, release_exn))
          | Termination_unconfirmed cancellation_exn ->
            remove_active pool active;
            release_active_after_completion active;
            Task_failed (payload, cancellation_exn))
      in
      complete_pool_task pool completion)

let rec worker_loop pool =
  let queued =
    Child.with_lock pool.mutex (fun () ->
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
      active_actions = [];
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
    Child.with_lock pool.mutex (fun () ->
        pool.stopping <- true;
        Condition.broadcast pool.work_available);
    List.iter Domain.join pool.workers;
    raise exn

let submit_pool_task pool payload task =
  match task.kind with
  | In_process run ->
    let stopping = Child.with_lock pool.mutex (fun () -> pool.stopping) in
    if stopping then raise (Error "subprocess worker pool is stopping");
    let completion =
      try Task_completed (payload, task.on_result (run ()))
      with exn -> Task_failed (payload, exn)
    in
    complete_pool_task pool completion
  | External _ | Concurrent _ ->
    Child.with_lock pool.mutex (fun () ->
        if pool.stopping then raise (Error "subprocess worker pool is stopping");
        Queue.add (payload, task) pool.queued;
        Condition.signal pool.work_available)

let await_pool_completion ~poll pool =
  let rec wait generation =
    match
      Child.with_lock pool.mutex (fun () ->
          if Queue.is_empty pool.completed_tasks then None
          else Some (Queue.take pool.completed_tasks))
    with
    | Some completion -> completion
    | None ->
      poll ();
      Child.await_notification pool.notifier generation |> wait
  in
  Child.notifier_generation pool.notifier |> wait

let stop_worker_pool ~cancel pool =
  let active, actions =
    Child.with_lock pool.mutex (fun () ->
        pool.stopping <- true;
        Queue.clear pool.queued;
        Condition.broadcast pool.work_available;
        let children =
          if cancel then (
            pool.signalling_cancellation <- true;
            pool.active_children
            |> List.filter_map (fun active ->
                match active.cancellation with
                | Cancellation_requested | Cancellation_failed _ -> None
                | Cancellation_not_requested ->
                  active.cancellation <- Cancellation_requested;
                  Some active.child))
          else []
        in
        let actions =
          if cancel then
            List.map (fun active -> active.cancel) pool.active_actions
          else []
        in
        (children, actions))
  in
  let signal_error =
    try
      Child.signal_running active;
      None
    with exn -> Some exn
  in
  let action_error =
    List.fold_left
      (fun first_error cancel ->
        try
          cancel ();
          first_error
        with exn -> (
          match first_error with
          | Some _ -> first_error
          | None -> Some exn))
      None actions
  in
  let cancellation_error =
    match signal_error with
    | Some _ -> signal_error
    | None -> action_error
  in
  Child.with_lock pool.mutex (fun () ->
      Option.iter
        (fun exn ->
          List.iter
            (fun active ->
              match active.cancellation with
              | Cancellation_requested ->
                active.cancellation <- Cancellation_failed exn
              | Cancellation_not_requested | Cancellation_failed _ -> ())
            pool.active_children)
        cancellation_error;
      pool.signalling_cancellation <- false;
      Condition.broadcast pool.cancellation_finished);
  Option.iter
    (fun _ -> Child.notify_completion pool.notifier)
    cancellation_error;
  List.iter Domain.join pool.workers;
  Option.iter raise cancellation_error

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

let run_dependency_graph_with_notifier ~max_jobs ~on_failure ~poll notifier
    works ~next =
  let graph =
    Graph.create_index works
      ~name:(fun work -> work.key)
      ~deps:(fun work -> work.dependencies)
      ~validation:
        (Graph.Reject_invalid
           {
             duplicate_node =
               (fun key -> Error ("duplicate subprocess work key: " ^ key));
             unknown_dependency =
               (fun ~node ~dependency ->
                 Error
                   (Printf.sprintf
                      "unknown dependency %s for subprocess work %s" dependency
                      node));
           })
  in
  let count = Graph.node_count graph in
  let pending = Hashtbl.create count in
  List.iter
    (fun work ->
      Hashtbl.add pending work.key (Graph.dependency_count graph work.key))
    works;
  let priorities = Hashtbl.create count in
  let remaining_dependents = Hashtbl.create count in
  let leaves = Queue.create () in
  List.iter
    (fun work ->
      let dependent_count = List.length (Graph.dependents graph work.key) in
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
    Graph.dependencies graph key
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
        Graph.shortest_cycle_in_index graph |> Option.value ~default:[]
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
    match on_failure exn with
    | Abort_immediately -> raise exn
    | Stop_new_work ->
      stopped := true;
      errors := (work.key, exn) :: !errors
    | Continue_independent_work -> errors := (work.key, exn) :: !errors
  in
  let complete work =
    incr completed;
    Graph.dependents graph work.key
    |> List.iter (fun dependent_key ->
        let remaining = Hashtbl.find pending dependent_key - 1 in
        Hashtbl.replace pending dependent_key remaining;
        if remaining = 0 then add_ready (Graph.find_node graph dependent_key))
  in
  let rec fill pool =
    (* Synchronous tasks and queued domain completions can keep the scheduler
       from reaching the wait path's poll. Check before admitting more work so
       an interrupt stops the next compiler request. *)
    poll ();
    if (not !stopped) && !in_flight < max_jobs then
      match Work_ready.min_elt_opt !ready with
      | None -> ()
      | Some ((_, key) as ready_key) ->
        ready := Work_ready.remove ready_key !ready;
        let work = Graph.find_node graph key in
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
    ?(on_failure =
      function
      | Sys.Break | Interrupted _ -> Abort_immediately
      | _ -> Stop_new_work) ?poll works ~next =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  match works with
  | [] -> ()
  | _ ->
    let poll, ticker_enabled =
      match poll with
      | Some poll -> (poll, true)
      | None -> ((fun () -> ()), false)
    in
    Child.with_completion_notifier ~ticker_enabled (fun notifier ->
        run_dependency_graph_with_notifier ~max_jobs ~on_failure ~poll notifier
          works ~next)

let run_tasks ?(max_jobs = default_max_jobs) ?poll ?(on_complete = fun _ -> ())
    tasks =
  let tasks = Array.of_list tasks in
  let results = Array.make (Array.length tasks) None in
  let works =
    Array.to_list
      (Array.mapi
         (fun index _ ->
           {
             key = Printf.sprintf "%010d" index;
             dependencies = [];
             value = index;
           })
         tasks)
  in
  run_dependency_graph ~max_jobs ?poll works ~next:(fun index result ->
      match result with
      | None -> Some tasks.(index)
      | Some result ->
        results.(index) <- Some result;
        on_complete index;
        None);
  Array.to_list results
  |> List.map (function
    | Some result -> result
    | None -> raise (Error "task result was not collected"))

let run_one ?poll ?stdout_chunk ?stderr_chunk ?stdin ?env
    ?(defer_signals = true) ~cwd program args =
  let poll, ticker_enabled =
    match poll with
    | Some poll -> (poll, true)
    | None -> ((fun () -> ()), false)
  in
  Child.with_completion_notifier ~defer_signals ~ticker_enabled (fun notifier ->
      let child =
        Child.launch ?stdout_chunk ?stderr_chunk ?stdin ?env ~defer_signals
          ~notifier () {program; args; cwd}
      in
      let completion_received = ref false in
      try
        let (_, result), deferred_signals =
          Child.wait_for_running ~poll ~defer_signals notifier [child]
        in
        completion_received := true;
        Signal_restore.protect deferred_signals (fun () ->
            Child.release_running child;
            result)
      with exn ->
        if not !completion_received then Child.terminate_running [child];
        raise exn)

let run ?poll ?defer_signals ~cwd program args =
  run_one ?poll ?defer_signals ~cwd program args

let run_task ?poll task =
  let result =
    match task.kind with
    | In_process run ->
      Option.iter (fun poll -> poll ()) poll;
      let result = run () in
      Option.iter (fun poll -> poll ()) poll;
      result
    | Concurrent {run; cancel = _} -> run ()
    | External {job; env} ->
      run_one ?poll ?env ~cwd:job.cwd job.program job.args
  in
  task.on_result result

let run_streaming ?poll ~cwd program args =
  let write channel bytes count =
    output channel bytes 0 count;
    flush channel
  in
  let stdin =
    if Platform.inherit_streaming_terminal_stdin || not (Unix.isatty Unix.stdin)
    then Child.Inherit_stdin
    else Child.Null_stdin
  in
  run_one ?poll ~stdout_chunk:(write stdout) ~stderr_chunk:(write stderr) ~stdin
    ~cwd program args
