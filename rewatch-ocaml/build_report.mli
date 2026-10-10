type t

(** Which output a build reports: watch builds print a completion line, and
    incremental rebuilds skip the cleanup phase. *)
type kind =
  | One_shot
  | Initial_watch
  | Incremental_watch
  | Full_watch
      (** Reporting owns presentation state independently of build execution. This
    keeps terminal progress, timing, and final diagnostics from influencing
    cleanup or retained-state transitions. *)

val create :
  started_at:float ->
  interactive:bool ->
  show_progress:bool ->
  colors:bool ->
  no_timing:bool ->
  kind:kind ->
  attempt:Build_attempt.t ->
  t

val report : t -> success:bool -> compile_seconds:float -> unit
val print_success_details : t -> compile_seconds:float -> string list
val report_completion : t -> string list -> unit
val report_parse_failure : t -> output:string -> unit
