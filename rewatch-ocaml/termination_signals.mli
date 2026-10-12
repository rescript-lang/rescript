val exit_code : int -> int
(** The shell's exit status for a process stopped by the signal. *)

val with_handlers : (int -> unit) -> (unit -> 'a) -> 'a
(** Runs the action with the handler installed for every
    {!Platform.termination_signals} entry, restoring the previous handlers
    afterwards. The first signal goes to the handler; a second one releases the
    held build locks and exits immediately. *)
