type t
(** Resolution caches package identity, not declaration-specific feature
    requests. The same installed package may be reached through multiple edges
    whose requested features must remain distinct for later aggregation. *)

type dependency = {
  name: string;
  directory: string;
  config: Config.t;
  is_local: bool;
}

type diagnostic_mode = Report_diagnostics | Suppress_diagnostics

val create : ?diagnostic_mode:diagnostic_mode -> Config.t -> t
val resolve : t -> package_root:string -> Config.dependency -> dependency
val dependency_path : t -> package_root:string -> string -> string option
val dependency_candidates : t -> package_root:string -> string -> string list
val root_package_name : t -> string
val is_local : t -> string -> bool
