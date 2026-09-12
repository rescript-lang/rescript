val run :
  package:Package_plan.t ->
  prepared:Build_session.prepared ->
  prepared_package:Package_plan.compilation ->
  attempt:Build_attempt.t ->
  watch:bool ->
  removed_module_names:(string, unit) Hashtbl.t ->
  parse_dirty_modules:(string, unit) Hashtbl.t ->
  unit
