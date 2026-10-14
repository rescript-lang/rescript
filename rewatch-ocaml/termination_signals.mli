val exit_code : int -> int
(** The shell's exit status for a process stopped by the signal. *)

val with_handlers : (int -> unit) -> (unit -> 'a) -> 'a
(** Runs the action with the handler installed for every
    {!Platform.termination_signals} entry, restoring the previous handlers
    afterwards. The handler only records the request: work stops at the next
    poll point, after the modules compiling in-process finish, so a stop never
    leaves a half-written artifact. Callers ignore repeated signals. *)
