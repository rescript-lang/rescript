let output_name = ref (None : string option) (* -o *)

and include_dirs = ref ([] : string list) (* -I *)

and debug = ref false (* -g *)

and fast = ref false (* -unsafe *)

and nopervasives = ref false (* -nopervasives *)

and preprocessor = ref (None : string option) (* -pp *)

and all_ppx = ref ([] : string list)

(* -ppx *)
let annotations = ref false (* -annot *)
let binary_annotations = ref false (* -annot *)

and noassert = ref false (* -noassert *)

and verbose = ref false (* -verbose *)

and open_modules = ref [] (* -open *)

and real_paths = ref true (* -short-paths *)

and applicative_functors = ref true (* -no-app-funct *)

and error_size = ref 500 (* -error-size *)

and transparent_modules = ref false (* -trans-mod *)
let dump_source = ref false (* -dsource *)
let dump_parsetree = ref false (* -dparsetree *)

and dump_typedtree = ref false (* -dtypedtree *)

and dump_rawlambda = ref false (* -drawlambda *)

and dump_lambda = ref false (* -dlambda *)

and only_parse = ref false (* -only-parse *)

and editor_mode = ref false (* -editor-mode *)

and ignore_parse_errors = ref false (* -ignore-parse-errors *)

let dont_write_files = ref false (* set to true under ocamldoc *)

let reset_dump_state () =
  dump_source := false;
  dump_parsetree := false;
  dump_typedtree := false;
  dump_rawlambda := false

let keep_locs = ref true (* -keep-locs *)

let parse_color_setting = function
  | "auto" -> Some Misc.Color.Auto
  | "always" -> Some Misc.Color.Always
  | "never" -> Some Misc.Color.Never
  | _ -> None
let color = ref None

(* -color *)

let unboxed_types = ref false

type mli_status = Mli_exists | Mli_non_exists
let assume_no_mli = ref Mli_non_exists
let dont_record_crc_unit : string option ref = ref None
let bs_gentype = ref false
let no_assert_false = ref false
let dump_location = ref true
