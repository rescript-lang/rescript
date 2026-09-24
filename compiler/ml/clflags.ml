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
  dont_record_crc_unit: string option ref;
  bs_gentype: bool ref;
  jsx_preserve: bool ref;
  no_assert_false: bool ref;
  dump_location: bool ref;
}

let create () =
  {
    output_name = ref None (* -o *);
    include_dirs = ref [] (* -I *);
    debug = ref false (* -g *);
    nopervasives = ref false (* -nopervasives *);
    all_ppx = ref [] (* -ppx *);
    binary_annotations = ref false (* write .cmt/.cmti; -bs-no-bin-annot *);
    noassert = ref false (* -noassert *);
    verbose = ref false (* -verbose *);
    open_modules = ref [] (* -open *);
    real_paths = ref true (* -short-paths *);
    transparent_modules = ref false (* -trans-mod *);
    dump_source = ref false (* -dsource *);
    dump_parsetree = ref false (* -dparsetree *);
    dump_typedtree = ref false (* -dtypedtree *);
    dump_rawlambda = ref false (* -drawlambda *);
    dump_coercions = ref false (* -draw-coercions *);
    only_parse = ref false (* -only-parse *);
    ignore_parse_errors = ref false (* -ignore-parse-errors *);
    dont_write_files = ref false;
    keep_locs = ref true (* -keep-locs *);
    color = ref None (* -color *);
    assume_no_mli = ref Mli_non_exists;
    dont_record_crc_unit = ref None;
    bs_gentype = ref false;
    jsx_preserve = ref false (* -bs-jsx-preserve *);
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
  state.dump_rawlambda := false;
  state.dump_coercions := false

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
  state.nopervasives := false;
  state.all_ppx := [];
  state.binary_annotations := true;
  state.noassert := false;
  state.verbose := false;
  state.open_modules := [];
  state.real_paths := true;
  state.transparent_modules := false;
  reset_dump_state ();
  state.only_parse := false;
  state.ignore_parse_errors := false;
  state.dont_write_files := false;
  state.keep_locs := true;
  state.color := Some Misc.Color.Always;
  state.assume_no_mli := Mli_non_exists;
  state.dont_record_crc_unit := None;
  state.bs_gentype := false;
  state.jsx_preserve := false;
  state.no_assert_false := false;
  state.dump_location := false
