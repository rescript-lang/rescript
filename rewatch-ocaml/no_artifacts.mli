(** A build that writes no artifact files (RESCRIPT_NO_ARTIFACTS=1).

    ASTs, interfaces, [.cmj] files and namespace maps of the packages a build
    compiles live in {!Build_file_store} only, for the lifetime of the
    process: a new process builds everything again, and a watch session keeps
    them across rebuilds. Only the JavaScript output, the compiler logs and
    the locks reach the disk. Typed trees are not produced (-bs-no-bin-annot),
    so editor tooling and gentype do not work in this mode. *)

val enabled : bool

val register_artifact_dir : string -> unit
(** Makes the artifacts of a package with this artifact directory live in
    memory. Call it before the build's requests run. *)

val install : unit -> unit
(** Installs the memory-only paths in {!Build_file_store} when the mode is
    enabled. *)
