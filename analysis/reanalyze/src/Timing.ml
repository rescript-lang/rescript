(** Timing utilities for measuring analysis phases *)

let enabled = ref false

type phase_times = {
  (* Churn (file add/remove) *)
  mutable churn: float;
  (* CMT processing sub-phases *)
  mutable file_loading: float;
  mutable result_collection: float;
  (* Analysis sub-phases *)
  mutable merging: float;
  mutable solving: float;
  (* Reporting *)
  mutable reporting: float;
}

let times =
  {
    churn = 0.0;
    file_loading = 0.0;
    result_collection = 0.0;
    merging = 0.0;
    solving = 0.0;
    reporting = 0.0;
  }

(* Mutex to protect timing updates from concurrent domains *)
let timing_mutex = Mutex.create ()

let reset () =
  times.churn <- 0.0;
  times.file_loading <- 0.0;
  times.result_collection <- 0.0;
  times.merging <- 0.0;
  times.solving <- 0.0;
  times.reporting <- 0.0

let add_churn_time t = times.churn <- times.churn +. t

let now () = Unix.gettimeofday ()

let time_phase phase_name f =
  if !enabled then (
    let start = now () in
    let result = f () in
    let elapsed = now () -. start in
    (* Use mutex to safely update shared timing state *)
    Mutex.lock timing_mutex;
    (match phase_name with
    | `FileLoading -> times.file_loading <- times.file_loading +. elapsed
    | `ResultCollection ->
      times.result_collection <- times.result_collection +. elapsed
    | `Merging -> times.merging <- times.merging +. elapsed
    | `Solving -> times.solving <- times.solving +. elapsed
    | `Reporting -> times.reporting <- times.reporting +. elapsed);
    Mutex.unlock timing_mutex;
    result)
  else f ()

let report () =
  if !enabled then (
    let cmt_total = times.file_loading in
    let analysis_total = times.merging +. times.solving in
    let total = times.churn +. cmt_total +. analysis_total +. times.reporting in
    Printf.eprintf "\n=== Timing ===\n";
    if times.churn > 0.0 then
      Printf.eprintf "  Churn:              %.3fs (%.1f%%)\n" times.churn
        (100.0 *. times.churn /. total);
    Printf.eprintf "  CMT processing:     %.3fs (%.1f%%)\n" cmt_total
      (100.0 *. cmt_total /. total);
    (* Only show parallel-specific timing when used *)
    if times.result_collection > 0.0 then
      Printf.eprintf
        "    - Parallel merge:   %.3fms (aggregate across domains)\n"
        (1000.0 *. times.result_collection);
    Printf.eprintf "  Analysis:           %.3fs (%.1f%%)\n" analysis_total
      (100.0 *. analysis_total /. total);
    Printf.eprintf "    - Merging:          %.3fms\n" (1000.0 *. times.merging);
    Printf.eprintf "    - Solving:          %.3fms\n" (1000.0 *. times.solving);
    Printf.eprintf "  Reporting:          %.3fs (%.1f%%)\n" times.reporting
      (100.0 *. times.reporting /. total);
    Printf.eprintf "  Total:              %.3fs\n" total)
