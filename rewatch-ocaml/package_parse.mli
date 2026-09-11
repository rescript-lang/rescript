val run :
  package:Build_types.graph_package ->
  stats:Build_types.t ->
  removed_module_names:(string, unit) Hashtbl.t ->
  (string, unit) Hashtbl.t
