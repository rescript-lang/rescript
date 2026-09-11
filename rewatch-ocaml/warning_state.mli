type entry = {
  module_name: string;
  package_root: string;
  path: string;
  output: string;
}

type t

val create : unit -> t

val set :
  t ->
  module_name:string ->
  package_root:string ->
  path:string ->
  output:string ->
  unit

val remove : t -> package_root:string -> path:string -> unit
val retain_paths : t -> string list -> unit
val entries : t -> entry list
