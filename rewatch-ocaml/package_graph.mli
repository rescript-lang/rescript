val source_discovery_prod : prod:bool -> is_local:bool -> bool

val discover :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  filter:Source_filter.t option ->
  attempt:Build_attempt.t ->
  Build_types.graph_package list
