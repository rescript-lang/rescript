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

let create () =
  {
    output_name = ref None;
    include_dirs = ref [];
    debug = ref false;
    fast = ref false;
    nopervasives = ref false;
    preprocessor = ref None;
    all_ppx = ref [];
    annotations = ref false;
    binary_annotations = ref false;
    noassert = ref false;
    verbose = ref false;
    open_modules = ref [];
    real_paths = ref true;
    applicative_functors = ref true;
    error_size = ref 500;
    transparent_modules = ref false;
    dump_source = ref false;
    dump_parsetree = ref false;
    dump_typedtree = ref false;
    dump_rawlambda = ref false;
    only_parse = ref false;
    editor_mode = ref false;
    ignore_parse_errors = ref false;
    dont_write_files = ref false;
    keep_locs = ref true;
    color = ref None;
    assume_no_mli = ref Mli_non_exists;
    dont_record_crc_unit = ref None;
    bs_gentype = ref false;
    no_assert_false = ref false;
    dump_location = ref true;
  }

(* Compiler flags are mutable during option parsing and source processing, so
   each request must own its refs. *)
let key = Domain.DLS.new_key create
let current () = Domain.DLS.get key

let with_fresh action =
  let previous = current () in
  Domain.DLS.set key (create ());
  Fun.protect action ~finally:(fun () -> Domain.DLS.set key previous)

let reset_dump_state () =
  let state = current () in
  state.dump_source := false;
  state.dump_parsetree := false;
  state.dump_typedtree := false;
  state.dump_rawlambda := false

let parse_color_setting = function
  | "auto" -> Some Misc.Color.Auto
  | "always" -> Some Misc.Color.Always
  | "never" -> Some Misc.Color.Never
  | _ -> None

let reset () =
  let state = current () in
  state.output_name := None;
  state.include_dirs := [];
  state.debug := true;
  state.fast := false;
  state.nopervasives := false;
  state.preprocessor := None;
  state.all_ppx := [];
  state.annotations := false;
  state.binary_annotations := true;
  state.noassert := false;
  state.verbose := false;
  state.open_modules := [];
  state.real_paths := true;
  state.applicative_functors := true;
  state.error_size := 500;
  state.transparent_modules := false;
  reset_dump_state ();
  state.only_parse := false;
  state.editor_mode := false;
  state.ignore_parse_errors := false;
  state.dont_write_files := false;
  state.keep_locs := true;
  state.color := Some Misc.Color.Always;
  state.assume_no_mli := Mli_non_exists;
  state.dont_record_crc_unit := None;
  state.bs_gentype := false;
  state.no_assert_false := false;
  state.dump_location := false
