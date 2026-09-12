val run :
  package:Build_types.graph_package ->
  prepared:Build_types.prepared ->
  prepared_package:Build_types.prepared_package ->
  attempt:Build_attempt.t ->
  removed_module_names:(string, unit) Hashtbl.t ->
  (string, unit) Hashtbl.t
