val prepare_tree :
  seen:(string, unit) Hashtbl.t ->
  package:Package_plan.t ->
  prepared:Build_session.prepared ->
  watch:bool ->
  attempt:Build_attempt.t ->
  unit
