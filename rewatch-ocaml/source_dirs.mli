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
