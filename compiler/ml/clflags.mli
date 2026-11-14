type mli_status = Mli_exists | Mli_non_exists

type t = {
  output_name: string option ref;
  include_dirs: string list ref;
  debug: bool ref;
  nopervasives: bool ref;
  all_ppx: string list ref;
  binary_annotations: bool ref;
  noassert: bool ref;
  verbose: bool ref;
  open_modules: string list ref;
  real_paths: bool ref;
  transparent_modules: bool ref;
  dump_source: bool ref;
  dump_parsetree: bool ref;
  dump_typedtree: bool ref;
  dump_rawlambda: bool ref;
  dump_coercions: bool ref;
  only_parse: bool ref;
  ignore_parse_errors: bool ref;
  dont_write_files: bool ref;
  keep_locs: bool ref;
  color: Misc.Color.setting option ref;
  assume_no_mli: mli_status ref;
  namespace: string option ref;
      (** Appended to the names of the outputs ([-bs-ns]): [Config] becomes
          [Config-Ns] *)
  alias_maps: string list ref;
      (** Opened alias maps, the namespace among them, whose interface CRCs are
          not recorded ([-bs-ns], [-bs-open-alias-map]) *)
  bs_gentype: bool ref;
  jsx_preserve: bool ref;
      (** Keep JSX in the output ([-bs-jsx-preserve]); read by translcore, and
          by js_dump through [Js_config] *)
  no_assert_false: bool ref;
  dump_location: bool ref;
}

val current : unit -> t
(* Mutable compiler flags for the current domain. *)

val parse_color_setting : string -> Misc.Color.setting option
val reset_dump_state : unit -> unit

val reset : unit -> unit
(** Sets bsc's defaults: debug information and binary annotations on, colored
    diagnostics, and no locations in dumps. Fresh flags start from OCaml's
    defaults instead, which editor analysis relies on. *)
