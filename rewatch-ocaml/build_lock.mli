type watch

val read_owner : string -> string option
val valid_owner : string -> bool

val with_build :
  ?poll:(unit -> unit) ->
  ?attempts:int ->
  string ->
  (release:(unit -> unit) -> 'a) ->
  'a
(** Waits for another build's lock, by default for up to a minute
    ([attempts] times 50 ms). *)

val with_watch : string -> (watch -> 'a) -> 'a

val release_held : unit -> unit
(** Releases every lock this process holds, for an exit that skips normal
    unwinding. *)

val is_owned : watch -> bool

module For_test : sig
  val publish : candidate:string -> path:string -> contents:string -> bool
end
