type hooks = {
  before_effect: unit -> unit;
      (** Called before the request writes a file (such as an .ast, .cmi,
          .cmt or .cmti) or runs a command. A build system that starts a
          request before its dependencies have finished uses it to wait for
          them; it may raise to abandon the request before any effect. *)
  interface_computed: string -> string -> unit;
      (** Called with the path of a .cmi the request is about to write, and
          the file's contents, when no error is pending. Called before
          {!before_effect}. *)
  redirect_cmi: string -> string option;
      (** The bytes to read instead of the .cmi file at a resolved candidate
          path. The name the compiler records stays the candidate's. *)
}
(** Callbacks a build system attaches to one in-process request. *)

val no_hooks : hooks

type t = {
  active: bool;
  hooks: hooks;
  cwd: string;
  mutable input_name: string;
  mutable command_runner: string -> int;
  mutable cmt_args: string array;
  mutable lambda_raise_count: int;
  mutable lambda_negative_raise_count: int;
  mutable builtin_ppx_local_module_counter: int;
  mutable parmatch_name_counter: int;
  mutable substitution_saved_id: int;
  mutable type_node_id: int;
  mutable type_node_last_snapshot: int;
  mutable runtime_path_override: string option;
  mutable project_root: string option;
  mutable load_path: string list;
}
(** Mutable state owned by one compiler request on one domain. Add fields here
    as process-global compiler state is moved behind request-aware accessors. *)

val current : unit -> t

val cwd : unit -> string
(** Logical working directory of this request, independent of [Sys.chdir]. *)

val resolve_path : string -> string
(** Resolve a relative file path against the active request root. Outside a
    request, preserve the path so ordinary process-relative I/O is unchanged. *)

val before_effect : unit -> unit
val interface_computed : string -> string -> unit
val redirect_cmi : string -> string option

val with_fresh : ?cwd:string -> ?hooks:hooks -> (unit -> 'a) -> 'a
(** Run with new request state and restore the prior state on success or failure. *)
