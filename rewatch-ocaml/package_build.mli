exception Parse_failure of string

val prepare_tree :
  root_config:Config.t ->
  dependency_context:Project_context.dependency_context ->
  seen:(string, unit) Hashtbl.t ->
  folder:string ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  watch:bool ->
  filter:string option ->
  is_local:bool ->
  stats:Build_types.t ->
  unit
