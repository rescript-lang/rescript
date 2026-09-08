let line_clear = "\027[2K\r"

let finished_compilation_message ~kind ~warnings ~seconds =
  let status = if warnings then "⚠️ " else "✅ " in
  let kind = Option.fold ~none:"" ~some:(fun value -> value ^ " ") kind in
  let warning_suffix = if warnings then " with warnings" else "" in
  Printf.sprintf "%s%sFinished %scompilation%s in %.2fs" line_clear status
    kind warning_suffix seconds

let should_clear_screen ~clear_screen ~interactive =
  clear_screen && interactive
