val run :
  ?poll:(unit -> unit) ->
  verbosity:int ->
  stdin:bool ->
  across_files:bool ->
  string ->
  bool
(** Type-checks a source file, or the contents read from stdin in its place,
    and prints the compiler's diagnostics for it. Returns whether the type
    check succeeded. The interface and typed tree of the check are written to
    lib/rescript/incremental for editor analysis; build artifacts are not
    changed. With [across_files], interfaces from earlier type checks in that
    directory take precedence over the build's. *)
