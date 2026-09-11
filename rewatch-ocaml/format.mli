exception Error of string

val write_file : string -> string -> unit
val files_in_scope : unit -> string list
val formatting_error : string -> string -> string
val format_check_summary : int -> string

val format_files_with_bsc :
  ?max_jobs:int -> bsc:string -> check:bool -> string list -> unit

val format_stdin : string -> unit
val run_files : check:bool -> string list -> unit
