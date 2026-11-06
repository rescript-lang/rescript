open OUnit2

let check condition message = assert_bool message condition

let work key dependencies = Process.{key; dependencies; value = key}

let result stdout = Process.{status = Unix.WEXITED 0; stdout; stderr = ""}

(* Waits for [condition] on a worker domain without hanging the suite. *)
let await ?(seconds = 10.) condition =
  let deadline = Unix.gettimeofday () +. seconds in
  while (not (condition ())) && Unix.gettimeofday () < deadline do
    Thread.delay 0.001
  done;
  condition ()

let concurrent run = Process.concurrent_task ~cancel:ignore run

(* A speculative task as Compiler_process builds one: it computes, then waits
   at its gate before its result may become visible. *)
let gated_task ?(on_cancel = ignore) gate ~started stdout =
  Process.concurrent_task
    ~cancel:(fun () ->
      on_cancel ();
      Speculation_gate.cancel gate)
    (fun () ->
      Atomic.set started true;
      Speculation_gate.wait gate;
      result stdout)

type graph_log = {mutable events: string list; mutable abandoned: string list}

let new_log () = {events = []; abandoned = []}
let record log event = log.events <- event :: log.events
let events log = List.rev log.events

(* [b] depends on [a]; [b] speculates while [a] runs. *)
let adopted_speculation_tests _context =
  let log = new_log () in
  let b_started = Atomic.make false in
  let gates = Hashtbl.create 2 in
  let mailbox = Process.create_mailbox () in
  Process.run_dependency_graph ~max_jobs:2
    [work "a" []; work "b" ["a"]]
    ~speculation:
      Process.
        {
          mailbox;
          candidates = (fun () -> ["b"]);
          start =
            (fun key ->
              record log ("speculate " ^ key);
              let gate = Speculation_gate.create () in
              Hashtbl.replace gates key gate;
              Some (gated_task gate ~started:b_started key));
          abandon = (fun key -> log.abandoned <- key :: log.abandoned);
        }
    ~next:(fun key outcome ->
      match (key, outcome) with
      | "a", None ->
        Some
          (concurrent (fun () ->
               (* The dependent starts while this work still runs. *)
               ignore (await (fun () -> Atomic.get b_started));
               result "a"))
      | "b", None ->
        record log "adopt b";
        Speculation_gate.open_ (Hashtbl.find gates "b");
        Some Process.adopt_speculation
      | key, Some result ->
        record log ("complete " ^ key ^ " " ^ result.Process.stdout);
        None
      | _, None -> assert false);
  check
    (events log = ["speculate b"; "complete a a"; "adopt b"; "complete b b"])
    "a speculation starts early and its result is processed once adopted";
  check (log.abandoned = []) "an adopted speculation is not abandoned"

let finished_speculation_is_held_tests _context =
  let log = new_log () in
  let b_finished = Atomic.make false in
  let mailbox = Process.create_mailbox () in
  Process.run_dependency_graph ~max_jobs:2
    [work "a" []; work "b" ["a"]]
    ~speculation:
      Process.
        {
          mailbox;
          candidates = (fun () -> ["b"]);
          start =
            (fun _ ->
              Some
                (concurrent (fun () ->
                     Atomic.set b_finished true;
                     result "early")));
          abandon = (fun key -> log.abandoned <- key :: log.abandoned);
        }
    ~next:(fun key outcome ->
      match (key, outcome) with
      | "a", None ->
        Some
          (concurrent (fun () ->
               ignore (await (fun () -> Atomic.get b_finished));
               (* Let the scheduler receive b's result first. *)
               Thread.delay 0.05;
               result "a"))
      | "b", None -> Some Process.adopt_speculation
      | key, Some result ->
        record log ("complete " ^ key ^ " " ^ result.Process.stdout);
        None
      | _, None -> assert false);
  check
    (events log = ["complete a a"; "complete b early"])
    "a speculation that finished early is held until its work is ready"

let unadopted_speculation_tests _context =
  let gates_for_abandon = Hashtbl.create 4 in
  let log = new_log () in
  let b_started = Atomic.make false in
  let b_cancelled_at_gate = Atomic.make false in
  let mailbox = Process.create_mailbox () in
  Process.run_dependency_graph ~max_jobs:2
    [work "a" []; work "b" ["a"]; work "c" ["b"]]
    ~speculation:
      Process.
        {
          mailbox;
          candidates = (fun () -> ["b"]);
          start =
            (fun key ->
              let gate = Speculation_gate.create () in
              Hashtbl.replace gates_for_abandon key gate;
              Some
                (Process.concurrent_task ~cancel:ignore (fun () ->
                     Atomic.set b_started true;
                     (try Speculation_gate.wait gate
                      with Speculation_gate.Abandoned ->
                        Atomic.set b_cancelled_at_gate true);
                     result key)));
          abandon =
            (fun key ->
              log.abandoned <- key :: log.abandoned;
              Speculation_gate.abandon (Hashtbl.find gates_for_abandon key));
        }
    ~next:(fun key outcome ->
      match (key, outcome) with
      | "a", None ->
        Some
          (concurrent (fun () ->
               ignore (await (fun () -> Atomic.get b_started));
               result "a"))
      | "b", None ->
        (* Nothing to compile after all. *)
        None
      | "c", None -> Some (concurrent (fun () -> result "c"))
      | key, Some result ->
        record log ("complete " ^ key ^ " " ^ result.Process.stdout);
        None
      | _, None -> assert false);
  check (log.abandoned = ["b"]) "a speculation that is not adopted is abandoned";
  check
    (await (fun () -> Atomic.get b_cancelled_at_gate))
    "the abandoned speculation is released from its gate";
  check
    (events log = ["complete a a"; "complete c c"])
    "an abandoned speculation's result is discarded"

exception Prerequisite_failed

let failed_prerequisite_tests _context =
  let abandoned = ref [] in
  let started = Hashtbl.create 4 in
  let gates = Hashtbl.create 4 in
  let mailbox = Process.create_mailbox () in
  let raised =
    try
      Process.run_dependency_graph ~max_jobs:3
        ~on_failure:(fun _ -> Process.Continue_independent_work)
        [work "a" []; work "b" ["a"]; work "c" ["b"]]
        ~speculation:
          Process.
            {
              mailbox;
              candidates = (fun () -> ["b"; "c"]);
              start =
                (fun key ->
                  let gate = Speculation_gate.create () in
                  let flag = Atomic.make false in
                  Hashtbl.replace started key flag;
                  Hashtbl.replace gates key gate;
                  Some (gated_task gate ~started:flag key));
              abandon =
                (fun key ->
                  abandoned := key :: !abandoned;
                  Speculation_gate.abandon (Hashtbl.find gates key));
            }
        ~next:(fun key outcome ->
          match (key, outcome) with
          | "a", None ->
            Some
              (concurrent (fun () ->
                   ignore
                     (await (fun () ->
                          Hashtbl.mem started "c"
                          && Atomic.get (Hashtbl.find started "c")));
                   result "a"))
          | "a", Some _ -> raise Prerequisite_failed
          | _ -> assert false);
      false
    with Prerequisite_failed -> true
  in
  check raised "the prerequisite's failure is reported";
  check
    (List.sort String.compare !abandoned = ["b"; "c"])
    "speculations of every dependent of a failed work are abandoned"

let mailbox_tests _context =
  let candidate_ready = ref false in
  let b_started = Atomic.make false in
  let message_domain = ref None in
  let scheduler_domain = Domain.self () in
  let mailbox = Process.create_mailbox () in
  let gates = Hashtbl.create 1 in
  Process.run_dependency_graph ~max_jobs:2
    [work "a" []; work "b" ["a"]]
    ~speculation:
      Process.
        {
          mailbox;
          candidates = (fun () -> if !candidate_ready then ["b"] else []);
          start =
            (fun key ->
              let gate = Speculation_gate.create () in
              Hashtbl.replace gates key gate;
              Some (gated_task gate ~started:b_started key));
          abandon = (fun _ -> ());
        }
    ~next:(fun key outcome ->
      match (key, outcome) with
      | "a", None ->
        Some
          (concurrent (fun () ->
               Process.post mailbox (fun () ->
                   message_domain := Some (Domain.self ());
                   candidate_ready := true);
               ignore (await (fun () -> Atomic.get b_started));
               result "a"))
      | "b", None ->
        Speculation_gate.open_ (Hashtbl.find gates "b");
        Some Process.adopt_speculation
      | _, Some _ -> None
      | _, None -> assert false);
  check (Atomic.get b_started) "a posted message lets a speculation start";
  check
    (!message_domain = Some scheduler_domain)
    "mailbox messages run on the scheduling domain"

let interrupted_speculation_tests _context =
  let b_started = Atomic.make false in
  let b_cancelled = Atomic.make false in
  let mailbox = Process.create_mailbox () in
  let started = Unix.gettimeofday () in
  let interrupted =
    try
      Process.run_dependency_graph ~max_jobs:2
        ~poll:(fun () ->
          if Atomic.get b_started then raise (Process.Interrupted 130))
        [work "a" []; work "b" ["a"]]
        ~speculation:
          Process.
            {
              mailbox;
              candidates = (fun () -> ["b"]);
              start =
                (fun key ->
                  let gate = Speculation_gate.create () in
                  Some
                    (gated_task
                       ~on_cancel:(fun () -> Atomic.set b_cancelled true)
                       gate ~started:b_started key));
              abandon = (fun _ -> ());
            }
        ~next:(fun key outcome ->
          match (key, outcome) with
          | "a", None ->
            Some
              (concurrent (fun () ->
                   ignore (await (fun () -> Atomic.get b_cancelled));
                   result "a"))
          | _ -> None);
      false
    with Process.Interrupted 130 -> true
  in
  check interrupted "an interrupt stops a graph with a waiting speculation";
  check (Atomic.get b_cancelled) "the waiting speculation is cancelled";
  check
    (Unix.gettimeofday () -. started < 5.)
    "cancellation releases the speculation's gate"

let gate_tests _context =
  let gate = Speculation_gate.create () in
  Speculation_gate.open_ gate;
  Speculation_gate.wait gate;
  Speculation_gate.abandon gate;
  Speculation_gate.wait gate;
  check (not (Speculation_gate.is_abandoned gate)) "an open gate stays open";
  let abandoned = Speculation_gate.create () in
  Speculation_gate.abandon abandoned;
  check
    (try
       Speculation_gate.wait abandoned;
       false
     with Speculation_gate.Abandoned -> true)
    "waiting on an abandoned gate raises";
  check (Speculation_gate.is_abandoned abandoned) "an abandoned gate reports it"

exception Timed_out

(* Fails a test that would otherwise hang. *)
let deadline_poll () =
  let deadline = Unix.gettimeofday () +. 10. in
  fun () -> if Unix.gettimeofday () > deadline then raise Timed_out

exception Adoption_failed

let stop_after_failed_adoption_tests _context =
  let started = ref [] in
  let a_started = Atomic.make false in
  let gates = Hashtbl.create 2 in
  let mailbox = Process.create_mailbox () in
  let raised =
    try
      Process.run_dependency_graph ~max_jobs:3 ~poll:(deadline_poll ())
        ~on_failure:(function
          | Adoption_failed -> Process.Stop_new_work
          | _ -> Process.Abort_immediately)
        [work "r" []; work "a" ["r"]; work "b" ["r"]]
        ~speculation:
          Process.
            {
              mailbox;
              candidates = (fun () -> ["a"]);
              start =
                (fun key ->
                  let gate = Speculation_gate.create () in
                  Hashtbl.replace gates key gate;
                  Some (gated_task gate ~started:a_started key));
              abandon =
                (fun key -> Speculation_gate.abandon (Hashtbl.find gates key));
            }
        ~next:(fun key outcome ->
          match (key, outcome) with
          | "r", None ->
            Some
              (concurrent (fun () ->
                   ignore (await (fun () -> Atomic.get a_started));
                   result "r"))
          | "a", None -> raise Adoption_failed
          | key, None ->
            started := key :: !started;
            Some (concurrent (fun () -> result key))
          | _, Some _ -> None);
      false
    with Adoption_failed -> true
  in
  check raised "a failed adoption is reported";
  check (!started = []) "no ordinary work starts after a stopping failure"

let held_result_continues_first_tests _context =
  let log = new_log () in
  let gates = Hashtbl.create 4 in
  let flags = Hashtbl.create 4 in
  let mailbox = Process.create_mailbox () in
  Process.run_dependency_graph ~max_jobs:2 ~poll:(deadline_poll ())
    [work "r" []; work "a" ["r"]; work "b" ["a"]; work "c" ["a"]]
    ~speculation:
      Process.
        {
          mailbox;
          candidates = (fun () -> ["a"; "b"; "c"]);
          start =
            (fun key ->
              let flag = Atomic.make false in
              Hashtbl.replace flags key flag;
              if key = "a" then
                Some
                  (concurrent (fun () ->
                       Atomic.set flag true;
                       result "a"))
              else
                let gate = Speculation_gate.create () in
                Hashtbl.replace gates key gate;
                Some (gated_task gate ~started:flag key));
          abandon =
            (fun key ->
              Option.iter Speculation_gate.abandon (Hashtbl.find_opt gates key));
        }
    ~next:(fun key outcome ->
      match (key, outcome) with
      | "r", None ->
        Some
          (concurrent (fun () ->
               ignore
                 (await (fun () ->
                      match Hashtbl.find_opt flags "a" with
                      | Some flag -> Atomic.get flag
                      | None -> false));
               Thread.delay 0.05;
               result "r"))
      | "a", None -> Some Process.adopt_speculation
      | "a", Some {Process.stdout = "a"} ->
        record log "a continues";
        Some (concurrent (fun () -> result "a-continued"))
      | key, None -> (
        match Hashtbl.find_opt gates key with
        | Some gate ->
          Speculation_gate.open_ gate;
          Some Process.adopt_speculation
        | None -> Some (concurrent (fun () -> result key)))
      | key, Some result ->
        record log ("complete " ^ key ^ " " ^ result.Process.stdout);
        None);
  check
    (List.mem "a continues" (events log)
    && List.mem "complete b b" (events log)
    && List.mem "complete c c" (events log))
    "an adopted result's next task runs before waiting dependents fill the \
     slots"

let tests =
  "process_speculation"
  >::: [
         "gate" >:: gate_tests;
         "adopted" >:: adopted_speculation_tests;
         "held" >:: finished_speculation_is_held_tests;
         "unadopted" >:: unadopted_speculation_tests;
         "failed_prerequisite" >:: failed_prerequisite_tests;
         "mailbox" >:: mailbox_tests;
         "interrupted" >:: interrupted_speculation_tests;
         "stop_after_failed_adoption" >:: stop_after_failed_adoption_tests;
         "held_result_continues_first" >:: held_result_continues_first_tests;
       ]
