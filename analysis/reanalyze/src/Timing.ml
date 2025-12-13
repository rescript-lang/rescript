(** Timing utilities for measuring analysis phases *)

let enabled = ref false

type phase_times = {
  mutable cmt_processing: float;
  mutable analysis: float;
  mutable reporting: float;
}

let times = {cmt_processing = 0.0; analysis = 0.0; reporting = 0.0}

let reset () =
  times.cmt_processing <- 0.0;
  times.analysis <- 0.0;
  times.reporting <- 0.0

let now () = Unix.gettimeofday ()

let time_phase phase_name f =
  if !enabled then (
    let start = now () in
    let result = f () in
    let elapsed = now () -. start in
    (match phase_name with
    | `CmtProcessing -> times.cmt_processing <- times.cmt_processing +. elapsed
    | `Analysis -> times.analysis <- times.analysis +. elapsed
    | `Reporting -> times.reporting <- times.reporting +. elapsed);
    result)
  else f ()

let report () =
  if !enabled then (
    let total = times.cmt_processing +. times.analysis +. times.reporting in
    Printf.eprintf "\n=== Timing ===\n";
    Printf.eprintf "  CMT processing: %.3fs (%.1f%%)\n" times.cmt_processing
      (100.0 *. times.cmt_processing /. total);
    Printf.eprintf "  Analysis:       %.3fs (%.1f%%)\n" times.analysis
      (100.0 *. times.analysis /. total);
    Printf.eprintf "  Reporting:      %.3fs (%.1f%%)\n" times.reporting
      (100.0 *. times.reporting /. total);
    Printf.eprintf "  Total:          %.3fs\n" total)
