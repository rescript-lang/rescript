val prepare_tree :
  seen:(string, unit) Hashtbl.t ->
  package:Build_types.graph_package ->
  prepared:Build_types.prepared ->
  watch:bool ->
  attempt:Build_attempt.t ->
  unit
