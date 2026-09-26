type result = {
  exit_code: int;
  stdout: string;
  stderr: string;
  diagnostics: Location.diagnostic list;
}
type session

val create_session : unit -> session

val set_session_frozen_enabled : session -> bool -> unit

val session_frozen_enabled : session -> bool
(** GenType projects retain the classic interface lookup while their typed
    output is being checked for frozen-lookup equivalence. The environment
    override can also disable this mode for an entire build. *)

val set_frozen_for_compile : session -> bool -> unit
(** Small incremental builds can skip frozen dependency lookup when only one
    module is ready to compile. Set before launching worker jobs. Capturing
    the compiled result remains enabled. *)

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

val publish_session_cmi :
  session -> retain:bool -> source:string -> destination:string -> unit
(** Make a successful staged interface available to later jobs after artifact
    publication. Both paths are absolute. [retain] skips freezing an unused
    leaf interface; a later new dependent can still load the disk artifact.
    An implementation with an explicit interface may have no new CMI; the
    published interface remains in force. *)

val stage_session_cmi : session -> source:string -> destination:string -> bool
(** Make a frozen CMI visible at its virtual destination before copying it.
    [false] means the producer supplied no valid freezable staged CMI. *)

val publish_session_cmj :
  session -> retain:bool -> source:string -> destination:string -> unit
(** Publish cross-module optimization metadata after its artifact is copied.
    Views handed to later requests own their mutable Lambda identifiers. *)

val stage_session_cmj : session -> source:string -> destination:string -> bool
(** Make frozen optimization metadata visible before copying its artifact. *)

val discard_pending_session_artifacts :
  session -> interface_file:string -> optimization_file:string option -> unit
(** Withdraw virtual outputs when export is cancelled or fails. *)

type fingerprint_kind = Interface | Optimization

val published_fingerprint :
  session -> kind:fingerprint_kind -> filename:string -> Digest.t option
(** Only returns a session fingerprint while the published artifact still has
    the same file identity. *)

val staged_ast_dependencies : session -> path:string -> string list option
(** The parser's dependency list before AST artifact publication. [path] is
    the absolute staging path. *)

val publish_session_ast : session -> source:string -> unit
(** Transfer a successful parse result to one later compiler request.
    [source] is the absolute staging path; persistent export may run later. *)

val publish_session_semantic :
  session -> retain:bool -> source:string -> destination:string -> unit
(** Retain a published typed result for future editor analysis. Results above
    4 MiB are skipped; the session keeps at most 64 results or 16 MiB of
    on-disk CMT size, evicting the oldest result first. *)

val semantic_result : session -> filename:string -> Cmt_format.cmt_infos option
(** Return an independent typed graph for a retained CMT path. A changed
    artifact or an evicted result returns [None], allowing disk fallback. *)

type module_result

val stage_module_result :
  session ->
  input:string ->
  interface_source:string ->
  interface_file:string ->
  optimization_source:string option ->
  optimization_file:string option ->
  semantic_source:string option ->
  dependencies:string list ->
  generated_outputs:string list ->
  unit
(** Publish one request's immutable result for dependent jobs before artifact
    export. Its source identities remain validated until export completes. *)

val publish_module_result :
  session ->
  input:string ->
  interface_file:string ->
  optimization_file:string option ->
  semantic_file:string option ->
  dependencies:string list ->
  generated_outputs:string list ->
  unit
(** Commit one complete module result after every output has been published.
    Paths are absolute. Failed or cancelled requests never reach this step. *)

val module_result : session -> interface_file:string -> module_result option
(** A published result is unavailable after its interface artifact changes. *)

val interface_fingerprint : module_result -> Digest.t option
val optimization_fingerprint : module_result -> Digest.t option
val interface_signature : module_result -> Types.signature option
val optimization_metadata : module_result -> Js_cmj_format.t option
val typed_semantic : module_result -> Cmt_format.cmt_infos option
val result_diagnostics : module_result -> Location.diagnostic list
val result_dependencies : module_result -> string list

val result_generated_outputs : module_result -> string list
(** Views of compiler graphs own their mutable nodes; metadata lists and
    fingerprints are immutable. Unretained leaves can use disk fallback. *)

val run : string array -> int
(** Shared command-line entry point used by the standalone [bsc] wrapper. *)
