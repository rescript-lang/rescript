type t

val create : stats:Build_types.t -> progress:Output.Progress.t -> t
val cleanup_artifacts : t -> unit
val finalize_logs : t -> unit
val finish : t -> unit
