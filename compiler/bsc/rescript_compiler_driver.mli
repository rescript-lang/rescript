type result = {exit_code: int; stdout: string; stderr: string}
type session

val create_session : unit -> session

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
    accidentally construct a request without a compilation input. Each request
    has fresh inference, environment, and diagnostic state. Compiler and
    external-command stdout and stderr are captured in the result. Ordinary
    argument, parse, type, and compilation outcomes are returned as an exit
    code and never terminate the host process. File I/O resolves against [cwd]
    without changing the process working directory. *)

val run_request_in_session :
  session ->
  run_external:(string -> int * string * string) option ->
  cwd:string ->
  argv:string list ->
  input:string ->
  result
(** Run a module job with project-owned dependency information. Each job still
    receives fresh inference and request state. *)

val run : string array -> int
(** Shared command-line entry point used by the standalone [bsc] wrapper. *)
