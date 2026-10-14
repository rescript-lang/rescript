type result = {exit_code: int; stdout: string; stderr: string}

val run_request :
  run_external:(string -> int * string * string) option ->
  cwd:string ->
  argv:string list ->
  input:string ->
  result
(** Run one compiler request in the logical working directory. [argv] contains
    only options; [input] is kept separate so build-system callers cannot
    accidentally construct a request without a compilation input. Requests may
    run concurrently on different domains: their compiler state is
    request-local. Compiler and external-command stdout and stderr are captured
    in the result, including for help, version, formatting, and reprinting
    requests. Ordinary argument, parse, type, and compilation outcomes are
    returned as an exit code and never terminate the host process; anything
    else raises {!Compiler_request_output.Failed} with the output so far. The request resolves file
    I/O against [cwd] without changing the process working directory. Request
    state is restored on success and failure. *)

val run : string array -> int
(** Command-line entry point of the standalone [bsc]. It writes to the process's
    stdout and stderr as it goes. *)
