val run :
  package:Build_types.graph_package ->
  stats:Build_types.t ->
  watch:bool ->
  removed_module_names:(string, unit) Hashtbl.t ->
  parse_dirty_modules:(string, unit) Hashtbl.t ->
  unit
