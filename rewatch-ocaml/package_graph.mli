val discover :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  filter:Source_filter.t option ->
  attempt:Build_attempt.t ->
  Package_plan.t list
