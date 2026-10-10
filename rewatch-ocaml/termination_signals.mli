val with_handlers : (int -> unit) -> (unit -> 'a) -> 'a
(** Runs the action with the handler installed for every
    {!Platform.termination_signals} entry, restoring the previous handlers
    afterwards. *)
