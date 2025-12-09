(** Cross-file items collected during AST processing.
    
    These are references that span file boundaries and need to be resolved
    after all files are processed. Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for processing after merge *)

(** {2 Types} *)

type t
(** Immutable cross-file items - for processing after merge *)

type builder
(** Mutable builder - for AST processing *)

(** {2 Builder API - for AST processing} *)

val create_builder : unit -> builder

val add_exception_ref :
  builder -> exception_path:Common.Path.t -> loc_from:Location.t -> unit
(** Add a cross-file exception reference (defined in another file). *)

val add_optional_arg_call :
  builder ->
  pos_to:Lexing.position ->
  arg_names:string list ->
  arg_names_maybe:string list ->
  unit
(** Add a cross-file optional argument call. *)

val add_function_reference :
  builder -> pos_from:Lexing.position -> pos_to:Lexing.position -> unit
(** Add a cross-file function reference (for optional args combining). *)

(** {2 Merge API} *)

val merge_all : builder list -> t
(** Merge all builders into one immutable result. Order doesn't matter. *)

(** {2 Processing API - for after merge} *)

val process_exception_refs :
  t ->
  refs:References.builder ->
  file_deps:FileDeps.builder ->
  find_exception:(Common.Path.t -> Location.t option) ->
  config:DceConfig.t ->
  unit
(** Process cross-file exception references. *)

(** {2 Optional Args State} *)

val compute_optional_args_state :
  t -> decls:Declarations.t -> Common.OptionalArgsState.t
(** Compute final optional args state from calls and function references.
    Pure function - does not mutate declarations. *)
