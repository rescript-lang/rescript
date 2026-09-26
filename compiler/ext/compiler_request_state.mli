type t = {
  active: bool;
  cwd: string;
  mutable input_name: string;
  mutable command_runner: string -> int;
  mutable cmt_args: string array;
  mutable lambda_raise_count: int;
  mutable lambda_negative_raise_count: int;
  mutable builtin_ppx_local_module_counter: int;
  mutable substitution_saved_id: int;
  mutable type_node_id: int;
  mutable type_node_reset_id: int option;
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

val is_regular_file : string -> bool
(** Test whether a request-relative path names a regular file. *)

val has_exact_directory_entry : string -> bool
(** Check the actual spelling of a directory entry, even on a case-insensitive
    filesystem with another spelling cached by the OS. *)

val canonical_output_path : string -> string
(** Resolve a path and canonicalize its existing parent directory, including
    when the output itself has not yet been exported. *)

val same_output_path : string -> string -> bool
(** Compare output locations, accounting for alternate case spellings of one
    existing file on case-insensitive filesystems. *)

val with_fresh : ?cwd:string -> (unit -> 'a) -> 'a
(** Run with new request state and restore the prior state on success or failure. *)
