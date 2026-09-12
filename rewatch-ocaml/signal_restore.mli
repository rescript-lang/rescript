type t
(** Signal restoration is explicit because termination is deferred across
    short ownership-transfer windows. [protect] restores exactly once and
    preserves the original exception if restoration also fails. *)

val create : defer:bool -> t
val restore : t -> unit
val exception_after_restore : t -> exn -> exn
val protect : t -> (unit -> 'a) -> 'a
