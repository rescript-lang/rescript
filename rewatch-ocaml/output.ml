let line_clear = "\027[2K\r"

let log ~minimum ~verbosity ~label message =
  if verbosity >= minimum then Printf.printf "%s:\n%s\n%!" label message

let debug ~verbosity message = log ~minimum:1 ~verbosity ~label:"DEBUG" message
let trace ~verbosity message = log ~minimum:2 ~verbosity ~label:"TRACE" message
let trace_enabled verbosity = verbosity >= 2

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

let cleanup_message ~step ~cleaned ~total ~seconds =
  Printf.sprintf "%s[%s] 🧹 Cleaned %d/%d in %.2fs" line_clear step cleaned
    total seconds

let compiler_cleanup_message ~step =
  Printf.sprintf "%s[%s] 🧹 Cleaned previous build due to compiler update"
    line_clear step

let parsing_message ~step ~count ~seconds =
  Printf.sprintf "%s[%s] 🧱 Parsed %d source files in %.2fs" line_clear step
    count seconds

let parsing_failed_message ~step ~seconds =
  Printf.sprintf "%s[%s] ❌ Error parsing source files in %.2fs" line_clear step
    seconds

let compiling_message ~step ~count ~seconds =
  Printf.sprintf "%s[%s] 🤺 Compiled %d modules in %.2fs" line_clear step
    count seconds

let compilation_failed_message ~step ~count ~seconds =
  Printf.sprintf "%s[%s] ❌ Compiled %d modules in %.2fs" line_clear step count
    seconds

let finished_compilation_message ~kind ~warnings ~seconds =
  let status = if warnings then "⚠️ " else "✅ " in
  let kind = Option.fold ~none:"" ~some:(fun value -> value ^ " ") kind in
  let warning_suffix = if warnings then " with warnings" else "" in
  Printf.sprintf "%s%sFinished %scompilation%s in %.2fs" line_clear status
    kind warning_suffix seconds

let should_clear_screen ~clear_screen ~show_progress ~interactive =
  clear_screen && show_progress && interactive
