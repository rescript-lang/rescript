val command : string -> int

val with_command_runner : (string -> int) -> (unit -> 'a) -> 'a
(** Temporarily route external preprocessor commands through a host-owned
    runner on the current domain. Long-lived compiler hosts use this to
    preserve cancellation and process-tree ownership. Nested scopes restore
    their previous runner even when the action raises. *)
