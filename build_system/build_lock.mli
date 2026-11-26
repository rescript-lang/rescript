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

val with_watch : running:(string -> string) -> string -> (watch -> 'a) -> 'a
(** [with_watch ~running root action] runs [action] holding the watch lock of
    [root], which [rescript watch] holds while it runs and [rescript clean]
    while it cleans. When a live process holds it, it fails with the message
    [running pid]. *)

val is_owned : watch -> bool

module For_test : sig
  val publish : candidate:string -> path:string -> contents:string -> bool
end
