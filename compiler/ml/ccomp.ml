let command cmdline =
  if !Clflags.verbose then (
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline ());
  Sys.command cmdline
