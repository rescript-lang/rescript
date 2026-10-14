let with_command_runner runner action =
  let state = Compiler_request_state.current () in
  let previous = state.command_runner in
  state.command_runner <- runner;
  Fun.protect action ~finally:(fun () -> state.command_runner <- previous)

let command cmdline =
  if !((Clflags.current ()).verbose) then (
    Compiler_request_output.write_stderr "+ ";
    Compiler_request_output.print_stderr cmdline);
  (Compiler_request_state.current ()).command_runner cmdline
