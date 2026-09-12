type t

val create : defer:bool -> t
val restore : t -> unit
val exception_after_restore : t -> exn -> exn
val protect : t -> (unit -> 'a) -> 'a
