type cleanup_result = {
  removed_modules: string list;
  present_public_outputs: (string, unit) Hashtbl.t;
}
(** Artifact cleanup first classifies ownership and builds a deletion plan.
    Public JavaScript, source maps, and compiler artifacts are then removed as
    one unit so an earlier deletion cannot erase evidence needed for a later
    ownership decision. *)

val artifact_dir : string -> string
(** [lib/rescript] below a package root. *)

val legacy_artifact_dirs : string -> string list
(** [lib/bs] and [lib/ocaml] below a package root, from ReScript 12 and
    earlier. *)

val clean : string -> unit
(** Removes the artifacts below a package root, except locks, and the legacy
    artifact directories. *)

val relative_output_directory : string -> Config.package_spec -> string

val ast_path : artifact_dir:string -> string -> string
(** The AST of a package-relative source path. *)

val invalidate_ast : string -> unit
(** Makes the next build parse the source of an AST again, without removing
    the AST. *)

val generated_js_path : Config.t -> string -> Config.package_spec -> string

val remove_public_outputs : Config.t -> string list -> unit

val cleanup_stale :
  artifact_files:string list ->
  ast_sources:Compile_assets.ast_source list ->
  present_source_files:string list ->
  on_removed_module:(string -> unit) ->
  root:string ->
  Config.t ->
  Source.module_ list ->
  cleanup_result
