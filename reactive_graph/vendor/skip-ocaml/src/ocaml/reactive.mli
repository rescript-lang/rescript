type 'a t

type filename = string
type key = string
type tracker
type 'a marshalled

val init : filename -> int -> unit
val input_files : filename array -> tracker t
val read_file : filename -> tracker -> string
val map : 'a t -> (key -> 'a array -> (key * 'b array) array) -> 'b t
val marshalled_map : 'a t -> (key -> 'a array -> (key * 'b array) array) -> 'b marshalled t
val unmarshal : 'a marshalled -> 'a
val get_array : 'a t -> key -> 'a array
val union : 'a t -> 'a t -> 'a t
val exit : unit -> unit
