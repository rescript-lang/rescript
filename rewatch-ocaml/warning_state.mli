type entry = {
  module_name: string;
  package_root: string;
  path: string;
  output: string;
}
(** Warning output survives retained watch attempts so unchanged warning-bearing
    modules can replay diagnostics. Watch shutdown uses the stored paths to
    invalidate freshness markers for the next process invocation. *)

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
