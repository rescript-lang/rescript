type t

type watch_path = {
  directory: string;
  recursive: bool;
}

type wait_result =
  | Changed
  | Stopped
  | Failed of string

val create : paths:watch_path list -> (t, string) result
val wait : t -> keep_running:(unit -> bool) -> wait_result
val refresh : t -> paths:watch_path list -> (unit, string) result
val close : t -> unit

module For_test : sig
  val handle_count : t -> int
end
