type result = {exit_code: int; stdout: string; stderr: string}

val build_identity : string
(** A digest of the compiler implementation linked into this driver. The
    standalone compiler and embedded callers therefore use the same identity
    without fingerprinting a separately installed executable. *)

val run_request :
  run_external:(string -> int * string * string) option ->
  cwd:string ->
  argv:string list ->
  input:string ->
  result
(** Run one compiler request in the logical working directory. [argv] contains
    only options; [input] is kept separate so build-system callers cannot
    accidentally construct a request without a compilation input. Requests are
    serialized by the caller because the native compiler owns global mutable
    state. Compiler and external-command stdout and stderr are captured in the
    result, including for help, version, formatting, and reprinting requests.
    Ordinary argument, parse, type, and compilation outcomes are returned as an
    exit code and never terminate the host process. The request resolves file
    I/O against [cwd] without changing the process working directory. Request
    state is restored on success and failure. *)

val run : string array -> int
(** Shared command-line entry point used by the standalone [bsc] wrapper. *)
