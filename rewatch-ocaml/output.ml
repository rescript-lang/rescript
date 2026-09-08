let line_clear = "\027[2K\r"

let cleanup_message ~step ~cleaned ~total ~seconds =
  Printf.sprintf "%s[%s] 🧹 Cleaned %d/%d in %.2fs" line_clear step cleaned
    total seconds

let compiler_cleanup_message ~step =
  Printf.sprintf "%s[%s] 🧹 Cleaned previous build due to compiler update"
    line_clear step

let parsing_message ~step ~count ~seconds =
  Printf.sprintf "%s[%s] 🧱 Parsed %d source files in %.2fs" line_clear step
    count seconds

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

let should_clear_screen ~clear_screen ~interactive =
  clear_screen && interactive
