type cycle_info = {
  cycle: string list;
  blocked: string list;
  modules_by_key: (string, Build_types.global_module) Hashtbl.t;
}

val resolved_dependencies :
  (string, Build_types.global_module) Hashtbl.t ->
  Build_types.global_module ->
  string list

val find_cycle :
  (string, Build_types.global_module) Hashtbl.t ->
  Build_state.t ->
  cycle_info option

val run :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  filter:string option ->
  watch:bool ->
  stats:Build_types.t ->
  parse_step:string ->
  on_cleanup:(float -> unit) ->
  cycle_info option
