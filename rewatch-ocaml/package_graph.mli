val discover :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  filter:Source_filter.t option ->
  attempt:Build_attempt.t ->
  Package_plan.t list
(** Package discovery resolves each dependency edge once, aggregates feature
    requests across all incoming edges, and then constructs stable package
    plans. Commands that need a different file projection reuse the lower-level
    {!Package_traversal} graph. *)
