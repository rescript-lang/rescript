type compilation_kind =
  | One_shot
  | Initial_watch
  | Incremental_watch
  | Full_watch

type t = {
  started_at: float;
  interactive: bool;
  show_progress: bool;
  colors: bool;
  no_timing: bool;
  compilation_kind: compilation_kind;
  stats: Build_types.t;
  finalize_logs: unit -> unit;
  write_metadata: unit -> unit;
}

let create ~started_at ~interactive ~show_progress ~colors ~no_timing
    ~compilation_kind ~stats ~finalize_logs ~write_metadata =
  {
    started_at;
    interactive;
    show_progress;
    colors;
    no_timing;
    compilation_kind;
    stats;
    finalize_logs;
    write_metadata;
  }

let compile_step report =
  match report.compilation_kind with
  | Incremental_watch -> "2/2"
  | _ -> "3/3"

let output_kind report =
  match report.compilation_kind with
  | Initial_watch -> Some "initial"
  | Incremental_watch -> Some "incremental"
  | One_shot | Full_watch -> None

let prepare report ~success ~compile_seconds =
  let stats = report.stats in
  report.finalize_logs ();
  if stats.attempt_kind = Build_types.Full_attempt then report.write_metadata ();
  if report.show_progress then
    if report.interactive then
      if success then
        print_endline
          (Output.compiling_message ~color:report.colors
             ~step:(compile_step report) ~count:stats.compiled
             ~seconds:compile_seconds)
      else
        prerr_endline
          (Output.compilation_failed_message ~color:report.colors
             ~step:(compile_step report) ~count:stats.compiled
             ~seconds:compile_seconds)
    else (
      (match report.compilation_kind with
      | One_shot | Initial_watch | Full_watch ->
        Printf.printf "Cleaned %d/%d\n%!" stats.cleaned stats.previous_asts
      | Incremental_watch -> ());
      Printf.printf "Parsed %d source files\n%!" stats.parsed;
      if success then Printf.printf "Compiled %d modules\n%!" stats.compiled
      else Printf.eprintf "Compiled %d modules\n%!" stats.compiled);
  let diagnostics =
    match report.compilation_kind with
    | Incremental_watch | Full_watch -> []
    | One_shot | Initial_watch ->
      stats.diagnostics |> List.rev |> List.sort_uniq String.compare
  in
  let warning_entries =
    Warning_state.entries (Build_types.warning_state stats)
  in
  List.iter
    (fun entry -> prerr_string entry.Warning_state.output)
    warning_entries;
  if warning_entries <> [] && diagnostics = [] then prerr_newline ();
  flush stderr;
  if diagnostics <> [] then
    diagnostics
    |> List.map (fun diagnostic ->
        if report.colors then Output.yellow diagnostic else diagnostic)
    |> String.concat "\n\n" |> prerr_endline;
  diagnostics

let report_completion report diagnostics =
  if report.interactive && report.show_progress then
    let seconds =
      if report.no_timing then 0. else Unix.gettimeofday () -. report.started_at
    in
    Printf.printf "\n%s\n%!"
      (Output.finished_compilation_message ~kind:(output_kind report)
         ~warnings:
           (report.stats.had_warnings || diagnostics <> []
           || Warning_state.entries (Build_types.warning_state report.stats)
              <> [])
         ~seconds)
  else if report.compilation_kind <> One_shot && report.show_progress then
    Printf.printf "Finished %scompilation\n%!"
      (match report.compilation_kind with
      | Initial_watch -> "initial "
      | Incremental_watch -> "incremental "
      | One_shot | Full_watch -> "")

let report report ~success ~compile_seconds =
  let diagnostics = prepare report ~success ~compile_seconds in
  if success then report_completion report diagnostics

let prepare_success report ~compile_seconds =
  prepare report ~success:true ~compile_seconds

let report_parse_failure report ~output =
  report.finalize_logs ();
  (if report.interactive && report.show_progress then
     prerr_endline
       (Output.parsing_failed_message ~color:report.colors
          ~step:
            (match report.compilation_kind with
            | Incremental_watch -> "1/2"
            | One_shot | Initial_watch | Full_watch -> "2/3")
          ~seconds:(if report.no_timing then 0. else report.stats.parse_seconds))
   else if report.show_progress then
     match report.compilation_kind with
     | One_shot | Initial_watch | Full_watch ->
       Printf.printf "Cleaned %d/%d\n%!" report.stats.cleaned
         report.stats.previous_asts
     | Incremental_watch -> ());
  prerr_endline output
