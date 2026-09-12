type t

val create :
  started_at:float ->
  interactive:bool ->
  show_progress:bool ->
  colors:bool ->
  no_timing:bool ->
  compilation_kind:Build_types.compilation_kind ->
  attempt:Build_attempt.t ->
  t

val report : t -> success:bool -> compile_seconds:float -> unit
val print_success_details : t -> compile_seconds:float -> string list
val report_completion : t -> string list -> unit
val report_parse_failure : t -> output:string -> unit
