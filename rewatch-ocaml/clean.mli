val run :
  root_config:Config.t ->
  dependency_context:Project_context.dependency_context ->
  seen:(string, unit) Hashtbl.t ->
  root:string ->
  prod:bool ->
  is_local:bool ->
  on_clean:(string -> unit) ->
  unit
