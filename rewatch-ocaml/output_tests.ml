let check condition message = if not condition then failwith message

let () =
  check
    (Output.finished_compilation_message ~kind:None ~warnings:false
       ~seconds:1.5
    = "\027[2K\r✅ Finished compilation in 1.50s")
    "clean completion format";
  check
    (Output.finished_compilation_message ~kind:(Some "incremental")
       ~warnings:true ~seconds:1.5
    = "\027[2K\r⚠️ Finished incremental compilation with warnings in 1.50s")
    "warning completion format";
  check
    (Output.should_clear_screen ~clear_screen:true ~interactive:true)
    "interactive clear-screen";
  check
    (not (Output.should_clear_screen ~clear_screen:true ~interactive:false))
    "non-interactive clear-screen";
  check
    (not (Output.should_clear_screen ~clear_screen:false ~interactive:true))
    "disabled clear-screen"
