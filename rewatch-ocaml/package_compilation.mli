val prepare :
  package:Package_plan.t ->
  prepared:Build_session.prepared ->
  prepared_package:Package_plan.compilation ->
  attempt:Build_attempt.t ->
  watch:bool ->
  removed_module_names:(string, unit) Hashtbl.t ->
  parse_dirty_modules:(string, unit) Hashtbl.t ->
  unit
(** Compilation preparation turns one stable package plan into the parse,
    namespace, and compiler work owned by the current attempt. It records
    pending work separately from scheduler eligibility so cycle-blocked modules
    remain dirty for a later recovery build. *)
