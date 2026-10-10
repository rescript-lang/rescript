val contains : string -> string -> bool
val strip_prefix : prefix:string -> string -> string

val strip_path : string -> string -> string
(** Removes the ["path: "] prefix that [Sys_error] messages carry, so callers
    can report the path in their own wording. *)
