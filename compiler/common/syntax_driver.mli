(** The compiler's side of parsing: reading sources through the build's file
    store, reporting syntax errors, and the entry points of the compiler. *)

val print_report :
  ?custom_intro:string option ->
  ?formatter:Format.formatter ->
  Res_diagnostics.t list ->
  string ->
  unit
(** Print syntax errors, in source order, with code frames from the source. *)

val report_warnings : Res_warning.t list -> unit
(** Report the parser's warnings as compiler warnings. *)

val parsing_engine : Res_diagnostics.t list Res_driver.parsing_engine
(** The parsing engine of the compiler, which reads through the build's file
    store. *)

exception Already_reported

val parse_implementation :
  ignore_parse_errors:bool -> string -> Parsetree.structure
[@@live] [@@throws Location.Error Already_reported]
(** Parse an implementation for the compiler, reporting syntax errors. *)

val parse_interface : ignore_parse_errors:bool -> string -> Parsetree.signature
[@@live] [@@throws Location.Error Already_reported]
(** Parse an interface for the compiler, reporting syntax errors. *)
