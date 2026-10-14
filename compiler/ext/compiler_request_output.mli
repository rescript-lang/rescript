val stdout_channel : unit -> out_channel
(** A real channel for binary or bulk stdout output. While capturing, it is
    backed by a temporary file, created on first use. *)

val is_active : unit -> bool
val stdout_formatter : unit -> Format.formatter
val stderr_formatter : unit -> Format.formatter
val write_stdout : string -> unit
val write_stderr : string -> unit
val print_stdout : string -> unit
val print_stderr : string -> unit

exception Failed of {error: exn; stdout: string; stderr: string}
(** Raised by {!with_capture} when the action raises [error]; carries the
    output written before it. *)

val with_capture : (unit -> 'a) -> 'a * string * string
(** Capture one domain's compiler output in memory and restore its previous
    streams. On Windows the captured text has CRLF line endings, as a bsc
    process's output would. *)
