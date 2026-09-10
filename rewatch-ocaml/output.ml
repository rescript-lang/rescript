let line_clear = "\027[2K\r"

let log ~minimum ~verbosity ~label message =
  if verbosity >= minimum then Printf.printf "%s:\n%s\n%!" label message

let debug ~verbosity message = log ~minimum:1 ~verbosity ~label:"DEBUG" message
let trace ~verbosity message = log ~minimum:2 ~verbosity ~label:"TRACE" message
let trace_enabled verbosity = verbosity >= 2

let format_step ~color step =
  if color then Printf.sprintf "\027[1m\027[2m[%s]\027[0m" step
  else Printf.sprintf "[%s]" step

module Progress = struct
  type phase = {
    step: string;
    symbol: string;
    label: string;
    total: int;
    mutable position: int;
  }

  type t = {
    enabled: bool;
    color: bool;
    mutable phase: phase option;
    mutable frame: int;
    mutable next_draw: float;
  }

  let frames = [|"⠁"; "⠂"; "⠄"; "⡀"; "⢀"; "⠠"; "⠐"; "⠈"|]
  let create ~enabled ~color =
    {enabled; color; phase = None; frame = 0; next_draw = 0.}

  let draw ?(force = false) progress =
    if progress.enabled then
      match progress.phase with
      | None -> ()
      | Some phase ->
        let now = Unix.gettimeofday () in
        if force || now >= progress.next_draw then (
          let spinner = frames.(progress.frame mod Array.length frames) in
          progress.frame <- progress.frame + 1;
          progress.next_draw <- now +. 0.08;
          Printf.printf "%s%s %s%s... %s %d/%d %!" line_clear
            (format_step ~color:progress.color phase.step)
            phase.symbol phase.label spinner phase.position phase.total)

  let start progress ~step ~symbol ~label ~total =
    progress.phase <- Some {step; symbol; label; total; position = 0};
    progress.frame <- 0;
    progress.next_draw <- 0.;
    draw ~force:true progress

  let advance progress =
    Option.iter
      (fun phase ->
        if phase.position < phase.total then
          phase.position <- phase.position + 1)
      progress.phase;
    draw progress

  let tick = draw
  let finish progress = progress.phase <- None

  let debug progress ~verbosity message =
    if verbosity >= 1 then (
      let redraw = progress.enabled && Option.is_some progress.phase in
      if redraw then Printf.printf "%s%!" line_clear;
      log ~minimum:1 ~verbosity ~label:"DEBUG" message;
      if redraw then draw ~force:true progress)

  let start_grouped progress ~step ~symbol ~label groups =
    let remaining = Hashtbl.create (List.length groups) in
    List.iter
      (fun group ->
        let count = Hashtbl.find_opt remaining group |> Option.value ~default:0 in
        Hashtbl.replace remaining group (count + 1))
      groups;
    start progress ~step ~symbol ~label ~total:(Hashtbl.length remaining);
    let groups = Array.of_list groups in
    fun index ->
      let group = groups.(index) in
      let count = Hashtbl.find remaining group - 1 in
      Hashtbl.replace remaining group count;
      if count = 0 then advance progress
end

let yellow text =
  if String.starts_with ~prefix:"\n" text then
    "\n\027[33m"
    ^ String.sub text 1 (String.length text - 1)
    ^ "\027[0m"
  else "\027[33m" ^ text ^ "\027[0m"

let colors_enabled_with ~getenv ~win32 ~interactive =
  let nonzero name default =
    match getenv name with
    | None -> default
    | Some value -> value <> "0"
  in
  let terminal_supports_color =
    interactive
    &&
    if win32 then true
    else
      Option.is_none (getenv "NO_COLOR")
      &&
      match getenv "TERM" with
      | Some term -> term <> "dumb"
      | None -> false
  in
  (terminal_supports_color && nonzero "CLICOLOR" true)
  || nonzero "CLICOLOR_FORCE" false

let colors_enabled ~interactive =
  colors_enabled_with ~getenv:Sys.getenv_opt ~win32:Sys.win32 ~interactive

let cleanup_message ~color ~step ~cleaned ~total ~seconds =
  Printf.sprintf "%s%s 🧹 Cleaned %d/%d in %.2fs" line_clear
    (format_step ~color step) cleaned total seconds

let compiler_cleanup_message ~color ~step =
  Printf.sprintf "%s%s 🧹 Cleaned previous build due to compiler update"
    line_clear (format_step ~color step)

let parsing_message ~color ~step ~count ~seconds =
  Printf.sprintf "%s%s 🧱 Parsed %d source files in %.2fs" line_clear
    (format_step ~color step) count seconds

let parsing_failed_message ~color ~step ~seconds =
  Printf.sprintf "%s%s ❌ Error parsing source files in %.2fs" line_clear
    (format_step ~color step) seconds

let compiling_message ~color ~step ~count ~seconds =
  Printf.sprintf "%s%s 🤺 Compiled %d modules in %.2fs" line_clear
    (format_step ~color step) count seconds

let compilation_failed_message ~color ~step ~count ~seconds =
  Printf.sprintf "%s%s ❌ Compiled %d modules in %.2fs" line_clear
    (format_step ~color step) count seconds

let finished_compilation_message ~kind ~warnings ~seconds =
  let status = if warnings then "⚠️ " else "✅ " in
  let kind = Option.fold ~none:"" ~some:(fun value -> value ^ " ") kind in
  let warning_suffix = if warnings then " with warnings" else "" in
  Printf.sprintf "%s%sFinished %scompilation%s in %.2fs" line_clear status
    kind warning_suffix seconds

let should_clear_screen ~clear_screen ~show_progress ~interactive =
  clear_screen && show_progress && interactive
