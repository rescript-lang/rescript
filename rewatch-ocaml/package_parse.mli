val run :
  package:Package_plan.t ->
  prepared:Build_session.prepared ->
  prepared_package:Package_plan.compilation ->
  attempt:Build_attempt.t ->
  removed_module_names:(string, unit) Hashtbl.t ->
  (string, unit) Hashtbl.t
