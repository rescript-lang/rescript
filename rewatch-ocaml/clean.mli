type t

val prepare :
  root_config:Config.t ->
  resolution:Package_resolution.t ->
  seen:(string, unit) Hashtbl.t ->
  prod:bool ->
  is_local:bool ->
  t

val remove_compiler_assets : t -> on_clean:(string -> unit) -> unit
val remove_generated_outputs : t -> unit
