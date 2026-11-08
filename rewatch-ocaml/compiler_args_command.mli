type context = {
  config: Config.t;  (** The configuration of the source's package. *)
  source: string;  (** The source, with its directory canonicalized. *)
  relative: string;  (** The source relative to [config.root]. *)
  contents: string;
  source_kind: Source.source_kind;
  has_interface: bool;
  dependency_dirs: string list;
      (** The artifact directories of the package's dependencies. *)
  runtime: string;
  project_root: string;
      (** The project root spelled the way the caller named the source. *)
}

val context :
  verbosity:int -> command:string -> ?contents:string -> string -> context
(** The compile context of a source file. [contents] replaces the file's
    contents, which are read otherwise. [command] names the command in
    errors. *)

val run : verbosity:int -> string -> string
