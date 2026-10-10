type t
(** Signal restoration is explicit because termination is deferred across
    short ownership-transfer windows. [protect] restores exactly once and
    preserves the original exception if restoration also fails. *)

val with_termination_handlers : (int -> unit) -> (unit -> 'a) -> 'a
(** Runs the action with the handler installed for every
    {!Platform.termination_signals} entry, restoring the previous handlers
    afterwards. *)

val create : defer:bool -> t
val restore : t -> unit
val exception_after_restore : t -> exn -> exn
val protect : t -> (unit -> 'a) -> 'a
