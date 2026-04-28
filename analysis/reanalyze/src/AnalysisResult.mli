(** Analysis result - immutable output from the solver.
    
    The solver returns this instead of logging directly.
    All side effects (logging, JSON output) happen in the reporting phase. *)

type t
(** Immutable analysis result *)

val empty : t
(** Empty result with no issues *)

val add_issue : t -> Issue.t -> t
(** Add a single issue to the result *)

val add_issues : t -> Issue.t list -> t
(** Add multiple issues to the result *)

val get_issues : t -> Issue.t list
(** Get all issues in order they were added *)

val issue_count : t -> int
(** Count of issues *)

(** {2 Issue constructors} *)

val make_dead_issue :
  loc:Location.t ->
  deadWarning:Issue.deadWarning ->
  path:string ->
  message:string ->
  Issue.t
(** Create a dead code warning issue *)

val make_dead_module_issue : loc:Location.t -> moduleName:Name.t -> Issue.t
(** Create a dead module warning issue *)
