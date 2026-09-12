val prepare_tree :
  seen:(string, unit) Hashtbl.t ->
  package:Build_types.graph_package ->
  watch:bool ->
  attempt:Build_attempt.t ->
  unit
