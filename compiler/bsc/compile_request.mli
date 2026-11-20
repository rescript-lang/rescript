(** A compiler request's options as a record. [of_argv] reads bsc's
    command-line flags into it, [to_argv] writes them, and [apply] applies them
    to the request's state. A field holds what the flags gave: a flag that was
    not given leaves its default, and repeatable flags keep their order. *)

(** Who sets a flag: the build system for every module, anyone (users through
    rescript.json or @@config, and the build system for its own settings),
    compiler developers inspecting a compile, or a command that does something
    other than compile. *)
type kind = Build_system | Public | Debug | Command

type jsx = {
  version: Js_config.jsx_version option;
  module_: string option;
  preserve: bool;
}

type source_map = {
  mode: Js_config.source_map option;
  sources_content: bool option;
  root: string option;
}

type gentype = {
  enabled: bool;
  module_: Gentype_config.module_ option;
  module_resolution: Gentype_config.module_resolution option;
  export_interfaces: bool;
  generated_extension: string option;
  suffix: string option;
  shims: (string * string) list;
  debug_items: string list;
  dependencies: string list;
  source_dirs: string list;
  dep_paths: (string * string) list;
  bsb_project_root: string option;
}

type codegen = {
  debug: bool;
  no_version_header: bool;
  cross_module_opt: bool option;
  no_check_div_by_zero: bool;
  no_assert_false: bool;
  noassert: bool;
  no_bin_annot: bool;
  keep_locs: bool option;
  short_paths: bool;
  no_builtin_ppx: bool;
  force_cmi: bool;
  force_cmj: bool;
  check_lam: bool;
  debug_ir: bool;
}

type stdlib = {nostdlib: bool; nopervasives: bool; make_runtime: bool}

type debug = {
  dparsetree: bool;
  dtypedtree: bool;
  drawlambda: bool;
  draw_coercions: bool;
  dsource: bool;
  locations: bool;
  only_parse: bool;
  syntax_only: bool;
  cmi_only: bool;
  ignore_parse_errors: bool;
  editor_mode: bool;
  as_pp: bool;
  test_ast_conversion: bool;
  verbose: bool;
}

type warning = {as_error: bool; spec: string}

type t = {
  include_dirs: string list;
  output: string option;
  read_cmi: bool;
  namespace: string option;
  package_name: string option;
  package_outputs: Js_packages_info.package_info list;
  project_root: string option;
  runtime_path: string option;
  ast_only: bool;
  transparent_modules: bool;
  absname: bool;
  color: Misc.Color.setting option;
  open_modules: string list;
  warnings: warning list;
  ppx: string list;
  experimental: Experimental_features.feature list;
  jsx: jsx;
  source_map: source_map;
  gentype: gentype;
  codegen: codegen;
  stdlib: stdlib;
  debug: debug;
}

val empty : t

type command =
  | Eval of string
  | Print_version
  | Warn_help
  | Format of string
  | Reprint_source of string
  | Help

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_flag_value of {flag: string; value: string; message: string}

type parsed = {request: t; commands: command list; arguments: string list}

val of_argv : ?base:t -> string list -> (parsed, error) result
(** Reads [argv] on top of [base]: flags into the request, commands and other
    arguments in order. Callers that accept only some kinds of flags check them
    with {!kind_of}. *)

val to_argv : t -> string list
(** The flags that [of_argv] reads back into the same request. *)

val apply : t -> unit
(** Applies the request on top of the current request's state, as its flags
    did: the runtime and package name before the package outputs, and the
    namespace before the other opened modules. *)

val absname : unit -> bool ref
(** [-absname], read when the compiler names a source file. *)

val flag_names : string list

val kind_of : string -> kind option

val takes_value : string -> bool option
(** Whether the flag takes a value, or [None] for an unknown flag. *)

val usage_entries : unit -> Bsc_args.t
(** The flags and their documentation, for the usage text. *)
