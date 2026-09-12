type scan = {
  build_root: string;
  scan_dirs: string list;
  also_scan_build_root: bool;
}

val write :
  root:string ->
  dirs:string list ->
  packages:(string * string) list ->
  scans:scan list ->
  unit

val write_build : root_config:Config.t -> Build_session.t -> unit
