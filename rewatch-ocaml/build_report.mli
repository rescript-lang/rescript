type compilation_kind =
  | One_shot
  | Initial_watch
  | Incremental_watch
  | Full_watch

type t

val create :
  started_at:float ->
  interactive:bool ->
  show_progress:bool ->
  colors:bool ->
  no_timing:bool ->
  compilation_kind:compilation_kind ->
  attempt:Build_attempt.t ->
  t

val report : t -> success:bool -> compile_seconds:float -> unit
val prepare_success : t -> compile_seconds:float -> string list
val report_completion : t -> string list -> unit
val report_parse_failure : t -> output:string -> unit
