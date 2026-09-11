type selection = All | Selected of string list
type t

val create : unit -> t
val add : t -> string -> string list option -> unit
val find : t -> string -> selection option
val mem : t -> string -> bool
val iter : t -> (string -> selection -> unit) -> unit
val to_option : selection -> string list option
