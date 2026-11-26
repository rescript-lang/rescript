type result = {
  prepared: Build_session.prepared;
  cycle: Module_graph.cycle_info option;
}

val run :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  filter:Source_filter.t option ->
  watch:bool ->
  attempt:Build_attempt.t ->
  parse_step:string ->
  on_cleanup:(float -> unit) ->
  result
