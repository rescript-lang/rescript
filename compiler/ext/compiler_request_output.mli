val stdout_channel : unit -> out_channel
val is_active : unit -> bool
val stderr_channel : unit -> out_channel
val stdout_formatter : unit -> Format.formatter
val stderr_formatter : unit -> Format.formatter
val write_stdout : string -> unit
val write_stderr : string -> unit
val print_stdout : string -> unit
val print_stderr : string -> unit

val with_capture : (unit -> 'a) -> 'a * string * string
(* Capture one domain's compiler output and restore its previous streams. *)
