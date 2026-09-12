val prepare_tree :
  seen:(string, unit) Hashtbl.t ->
  package:Build_types.graph_package ->
  watch:bool ->
  stats:Build_types.t ->
  unit
