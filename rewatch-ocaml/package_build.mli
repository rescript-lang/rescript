val prepare_tree :
  seen:(string, unit) Hashtbl.t ->
  package:Package_plan.t ->
  prepared:Build_session.prepared ->
  watch:bool ->
  attempt:Build_attempt.t ->
  unit
(** Package preparation remains recursive because dependencies must register
    their compiler work before a consumer can be scheduled. [seen] prevents a
    package graph cycle or duplicate dependency edge from preparing a package
    twice within the attempt. *)
