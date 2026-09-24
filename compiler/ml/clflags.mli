type mli_status = Mli_exists | Mli_non_exists

type t = {
  output_name: string option ref;
  include_dirs: string list ref;
  debug: bool ref;
  fast: bool ref;
  nopervasives: bool ref;
  preprocessor: string option ref;
  all_ppx: string list ref;
  annotations: bool ref;
  binary_annotations: bool ref;
  noassert: bool ref;
  verbose: bool ref;
  open_modules: string list ref;
  real_paths: bool ref;
  applicative_functors: bool ref;
  error_size: int ref;
  transparent_modules: bool ref;
  dump_source: bool ref;
  dump_parsetree: bool ref;
  dump_typedtree: bool ref;
  dump_rawlambda: bool ref;
  only_parse: bool ref;
  editor_mode: bool ref;
  ignore_parse_errors: bool ref;
  dont_write_files: bool ref;
  keep_locs: bool ref;
  color: Misc.Color.setting option ref;
  assume_no_mli: mli_status ref;
  dont_record_crc_unit: string option ref;
  bs_gentype: bool ref;
  no_assert_false: bool ref;
  dump_location: bool ref;
}

val current : unit -> t
(* Mutable compiler flags for the current domain. *)

val with_fresh : (unit -> 'a) -> 'a
(* Run with fresh flags and restore the previous request on exit. *)

val parse_color_setting : string -> Misc.Color.setting option
val reset_dump_state : unit -> unit
val reset : unit -> unit
