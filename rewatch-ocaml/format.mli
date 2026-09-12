exception Error of string

val write_file : string -> string -> unit
val formatting_error : string -> string -> string
val format_check_summary : int -> string

val format_files_with_bsc :
  ?max_jobs:int ->
  ?poll:(unit -> unit) ->
  bsc:string ->
  check:bool ->
  string list ->
  unit

val format_stdin : ?poll:(unit -> unit) -> string -> unit
val run_files : ?poll:(unit -> unit) -> check:bool -> string list -> unit
