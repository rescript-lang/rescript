(* Mark an AST stale so the build system reports its warnings again. *)
external mark_stale : string -> unit = "caml_stale_file"
