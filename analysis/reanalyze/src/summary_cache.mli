type error =
  | Io_error of string
  | Invalid_format of string
  | Digest_mismatch of {expected: string; actual: string}
  | Not_found

type result =
  | Hit
  | Refreshed

val cache_dir : project_root:string -> string
val path_for : project_root:string -> digest:string -> string

val write :
  project_root:string -> Summary.t -> (unit, error) Stdlib.result

val read :
  project_root:string -> digest:string -> (Summary.t, error) Stdlib.result

val read_or_recompute :
  project_root:string -> Summary.t -> (result, error) Stdlib.result

val error_message : error -> string

