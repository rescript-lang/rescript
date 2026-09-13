val run :
  package:Package_plan.t ->
  prepared:Build_session.prepared ->
  prepared_package:Package_plan.compilation ->
  attempt:Build_attempt.t ->
  removed_module_names:(string, unit) Hashtbl.t ->
  (string, unit) Hashtbl.t
(** Parsing returns the modules whose dependency information changed or whose
    compilation is pending. Failed and warning-bearing source paths are also
    retained in the session so later watch attempts cannot silently reuse an
    older AST. *)
