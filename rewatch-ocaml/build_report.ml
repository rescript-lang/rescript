type t = {
  started_at: float;
  interactive: bool;
  show_progress: bool;
  colors: bool;
  no_timing: bool;
  compilation_kind: Build_types.compilation_kind;
  attempt: Build_attempt.t;
}

let create ~started_at ~interactive ~show_progress ~colors ~no_timing
    ~compilation_kind ~attempt =
  {
    started_at;
    interactive;
    show_progress;
    colors;
    no_timing;
    compilation_kind;
    attempt;
  }

let compile_step report =
  match report.compilation_kind with
  | Build_types.Incremental_watch -> "2/2"
  | _ -> "3/3"

let output_kind report =
  match report.compilation_kind with
  | Build_types.Initial_watch -> Some "initial"
  | Build_types.Incremental_watch -> Some "incremental"
  | Build_types.One_shot | Build_types.Full_watch -> None

let prepare report ~success ~compile_seconds =
  let attempt = report.attempt in
  if report.show_progress then
    if report.interactive then
      if success then
        print_endline
          (Output.compiling_message ~color:report.colors
             ~step:(compile_step report) ~count:attempt.compiled
             ~seconds:compile_seconds)
      else
        prerr_endline
          (Output.compilation_failed_message ~color:report.colors
             ~step:(compile_step report) ~count:attempt.compiled
             ~seconds:compile_seconds)
    else (
      (match report.compilation_kind with
      | Build_types.One_shot | Build_types.Initial_watch
      | Build_types.Full_watch ->
        Printf.printf "Cleaned %d/%d\n%!" attempt.cleaned attempt.previous_asts
      | Build_types.Incremental_watch -> ());
      Printf.printf "Parsed %d source files\n%!" attempt.parsed;
      if success then Printf.printf "Compiled %d modules\n%!" attempt.compiled
      else Printf.eprintf "Compiled %d modules\n%!" attempt.compiled);
  let diagnostics =
    match report.compilation_kind with
    | Build_types.Incremental_watch | Build_types.Full_watch -> []
    | Build_types.One_shot | Build_types.Initial_watch ->
      attempt.diagnostics |> List.rev |> List.sort_uniq String.compare
  in
  let warning_entries =
    Warning_state.entries (Build_session.warning_state attempt.session)
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
           (report.attempt.had_warnings || diagnostics <> []
           || Warning_state.entries
                (Build_session.warning_state report.attempt.session)
              <> [])
         ~seconds)
  else if
    report.compilation_kind <> Build_types.One_shot && report.show_progress
  then
    Printf.printf "Finished %scompilation\n%!"
      (match report.compilation_kind with
      | Build_types.Initial_watch -> "initial "
      | Build_types.Incremental_watch -> "incremental "
      | Build_types.One_shot | Build_types.Full_watch -> "")

let report report ~success ~compile_seconds =
  let diagnostics = prepare report ~success ~compile_seconds in
  if success then report_completion report diagnostics

let print_success_details report ~compile_seconds =
  prepare report ~success:true ~compile_seconds

let report_parse_failure report ~output =
  (if report.interactive && report.show_progress then
     prerr_endline
       (Output.parsing_failed_message ~color:report.colors
          ~step:
            (match report.compilation_kind with
            | Build_types.Incremental_watch -> "1/2"
            | Build_types.One_shot | Build_types.Initial_watch
            | Build_types.Full_watch ->
              "2/3")
          ~seconds:
            (if report.no_timing then 0. else report.attempt.parse_seconds))
   else if report.show_progress then
     match report.compilation_kind with
     | Build_types.One_shot | Build_types.Initial_watch | Build_types.Full_watch
       ->
       Printf.printf "Cleaned %d/%d\n%!" report.attempt.cleaned
         report.attempt.previous_asts
     | Build_types.Incremental_watch -> ());
  prerr_endline output
