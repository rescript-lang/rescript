type t

type change_kind = Content | Structural
type change = {path: string option; kind: change_kind}

type watch_path = {
  directory: string;
  recursive: bool;
}

type wait_result =
  | Changed of change list
  | Stopped
  | Failed of string

val create : paths:watch_path list -> (t, string) result
val is_compiler_artifact_directory : string -> bool
val wait : t -> keep_running:(unit -> bool) -> wait_result
val drain : t -> change list
val watches_directory : t -> string -> bool
val refresh : t -> paths:watch_path list -> (unit, string) result
val close : t -> unit

module For_test : sig
  val handle_count : t -> int
  val directory_identity : t -> string -> string option
  val queue_change : t -> unit
  val queue_error : t -> string -> unit
end
