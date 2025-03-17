(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let absname = ref false

let set_abs_input_name sourcefile =
  let sourcefile =
    if !absname && Filename.is_relative sourcefile then
      Ext_path.absolute_cwd_path sourcefile
    else sourcefile
  in
  Location.set_input_name sourcefile;
  sourcefile
let setup_outcome_printer () = Lazy.force Res_outcome_printer.setup

let setup_runtime_path path =
  let u0 = Filename.dirname path in
  let std = Filename.basename path in
  let _path = Filename.dirname u0 in
  let rescript = Filename.basename u0 in
  (match rescript.[0] with
  | '@' ->
    (* scoped package *)
    Bs_version.package_name := rescript ^ "/" ^ std
  | _ -> Bs_version.package_name := std
  | exception _ -> Bs_version.package_name := std);
  Js_config.customize_runtime := Some path

let process_file sourcefile ?kind ppf =
  (* This is a better default then "", it will be changed later
     The {!Location.input_name} relies on that we write the binary ast
     properly
  *)
  setup_outcome_printer ();
  let kind =
    match kind with
    | None ->
      Ext_file_extensions.classify_input
        (Ext_filename.get_extension_maybe sourcefile)
    | Some kind -> kind
  in
  let res =
    match kind with
    | Res ->
      let sourcefile = set_abs_input_name sourcefile in
      Js_implementation.implementation
        ~parser:
          (Res_driver.parse_implementation
             ~ignore_parse_errors:!Clflags.ignore_parse_errors)
        ppf sourcefile
    | Resi ->
      let sourcefile = set_abs_input_name sourcefile in
      Js_implementation.interface
        ~parser:
          (Res_driver.parse_interface
             ~ignore_parse_errors:!Clflags.ignore_parse_errors)
        ppf sourcefile
    | Intf_ast -> Js_implementation.interface_mliast ppf sourcefile
    (* The printer setup is done in the runtime depends on
       the content of ast
    *)
    | Impl_ast -> Js_implementation.implementation_mlast ppf sourcefile
    | Mlmap ->
      Location.set_input_name sourcefile;
      Js_implementation.implementation_map ppf sourcefile
    | Cmi ->
      let cmi_sign = (Cmi_format.read_cmi sourcefile).cmi_sign in
      Printtyp.signature Format.std_formatter cmi_sign;
      Format.pp_print_newline Format.std_formatter ()
    | Unknown -> Bsc_args.bad_arg ("don't know what to do with " ^ sourcefile)
  in
  res

let reprint_source_file sourcefile =
  let kind =
    Ext_file_extensions.classify_input
      (Ext_filename.get_extension_maybe sourcefile)
  in
  let sourcefile = set_abs_input_name sourcefile in
  let res =
    match kind with
    | Res ->
      let parse_result =
        Res_driver.parsing_engine.parse_implementation ~for_printer:true
          ~filename:sourcefile
      in
      if parse_result.invalid then (
        Res_diagnostics.print_report parse_result.diagnostics
          parse_result.source;
        exit 1);
      Res_compmisc.init_path ();
      parse_result.parsetree
      |> Cmd_ppx_apply.apply_rewriters ~restore:false
           ~tool_name:Js_config.tool_name Ml
      |> Ppx_entry.rewrite_implementation
      |> Res_printer.print_implementation ~width:100
           ~comments:parse_result.comments
      |> print_endline
    | Resi ->
      let parse_result =
        Res_driver.parsing_engine.parse_interface ~for_printer:true
          ~filename:sourcefile
      in
      if parse_result.invalid then (
        Res_diagnostics.print_report parse_result.diagnostics
          parse_result.source;
        exit 1);
      Res_compmisc.init_path ();
      parse_result.parsetree
      |> Cmd_ppx_apply.apply_rewriters ~restore:false
           ~tool_name:Js_config.tool_name Mli
      |> Ppx_entry.rewrite_signature
      |> Res_printer.print_interface ~width:100 ~comments:parse_result.comments
      |> print_endline
    | _ ->
      print_endline
        ("Invalid input for reprinting ReScript source. Must be a ReScript \
          file: " ^ sourcefile);
      exit 2
  in
  res

let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)

let anonymous ~(rev_args : string list) =
  match rev_args with
  | [filename] -> process_file filename ppf
  | [] -> ()
  | _ ->
    if !Js_config.syntax_only then
      Ext_list.rev_iter rev_args (fun filename ->
          Clflags.reset_dump_state ();
          Warnings.reset ();
          process_file filename ppf)
    else Bsc_args.bad_arg "can not handle multiple files"

let format_file input =
  let ext =
    Ext_file_extensions.classify_input (Ext_filename.get_extension_maybe input)
  in
  (match ext with
  | Res | Resi -> ()
  | _ -> Bsc_args.bad_arg ("don't know what to do with " ^ input));
  let formatted =
    Res_multi_printer.print
      ~ignore_parse_errors:!Clflags.ignore_parse_errors
      input
  in
  match !Clflags.output_name with
  | None -> output_string stdout formatted
  | Some fname -> Ext_io.write_file fname formatted

let set_color_option option =
  match Clflags.parse_color_setting option with
  | None -> ()
  | Some setting -> Clflags.color := Some setting

let eval (s : string) ~suffix =
  let tmpfile = Filename.temp_file "eval" suffix in
  Ext_io.write_file tmpfile s;
  anonymous ~rev_args:[tmpfile];
  if not !Clflags.verbose then try Sys.remove tmpfile with _ -> ()

(* let (//) = Filename.concat *)

module Pp = Rescript_cpp
let define_variable s =
  match Ext_string.split ~keep_empty:true s '=' with
  | [key; v] ->
    if not (Pp.define_key_value key v) then
      Bsc_args.bad_arg ("illegal definition: " ^ s)
  | _ -> Bsc_args.bad_arg ("illegal definition: " ^ s)

let print_standard_library () =
  let ( // ) = Filename.concat in
  let standard_library =
    Filename.dirname Sys.executable_name
    // Filename.parent_dir_name // "lib" // "ocaml"
  in
  print_string standard_library;
  print_newline ();
  exit 0

let bs_version_string = "ReScript " ^ Bs_version.version

let print_version_string () =
  print_endline bs_version_string;
  exit 0

let[@inline] set s : Bsc_args.spec = Unit (Unit_set s)
let[@inline] clear s : Bsc_args.spec = Unit (Unit_clear s)
let[@inline] string_call s : Bsc_args.spec = String (String_call s)
let[@inline] string_optional_set s : Bsc_args.spec =
  String (String_optional_set s)

let[@inline] unit_call s : Bsc_args.spec = Unit (Unit_call s)
let[@inline] string_list_add s : Bsc_args.spec = String (String_list_add s)

(* mostly common used to list in the beginning to make search fast
*)
let buckle_script_flags : (string * Bsc_args.spec * string) array =
  [|
    ( "-I",
      string_list_add Clflags.include_dirs,
      "*internal* <dir>  Add <dir> to the list of include directories" );
    ( "-w",
      string_call (Warnings.parse_options false),
      "<list>  Enable or disable warnings according to <list>:\n\
       +<spec>   enable warnings in <spec>\n\
       -<spec>   disable warnings in <spec>\n\
       @<spec>   enable warnings in <spec> and treat them as errors\n\
       <spec> can be:\n\
       <num>             a single warning number\n\
       <num1>..<num2>    a range of consecutive warning numbers\n\
       default setting is " ^ Bsc_warnings.defaults_w );
    ( "-o",
      string_optional_set Clflags.output_name,
      "*internal* <file>  set output file name to <file>" );
    ( "-bs-read-cmi",
      unit_call (fun _ -> Clflags.assume_no_mli := Mli_exists),
      "*internal* Assume mli always exist " );
    ( "-ppx",
      string_list_add Clflags.all_ppx,
      "*internal* <command>  Pipe abstract syntax trees through preprocessor \
       <command>" );
    ( "-open",
      string_list_add Clflags.open_modules,
      "*internal* <module>  Opens the module <module> before typing" );
    ( "-bs-jsx",
      string_call (fun i ->
          if i <> "3" && i <> "4" then
            Bsc_args.bad_arg (" Not supported jsx version : " ^ i);
          Js_config.jsx_version :=
            Js_config.jsx_version_of_int @@ int_of_string i),
      "*internal* Set jsx version" );
    ( "-bs-jsx-module",
      string_call (fun i ->
          let is_generic =
            match i |> String.lowercase_ascii with
            | "react" -> false
            | _ -> true
          in
          Js_config.jsx_module := Js_config.jsx_module_of_string i;
          if is_generic then Js_config.jsx_version := Some Jsx_v4),
      "*internal* Set jsx module" );
    ( "-bs-jsx-mode",
      string_call ignore,
      "*internal* Set jsx mode, this is no longer used and is a no-op." );
    ( "-bs-package-output",
      string_call Js_packages_state.update_npm_package_path,
      "*internal* Set npm-output-path: [opt_module]:path, for example: \
       'lib/cjs', 'amdjs:lib/amdjs', 'es6:lib/es6' " );
    ( "-bs-ast",
      unit_call (fun _ ->
          Js_config.binary_ast := true;
          Js_config.syntax_only := true),
      "*internal* Generate binary .mli_ast and ml_ast and stop" );
    ( "-bs-syntax-only",
      set Js_config.syntax_only,
      "*internal* Only check syntax" );
    ( "-bs-g",
      unit_call (fun _ ->
          Js_config.debug := true;
          Pp.replace_directive_bool "DEBUG" true),
      "Debug mode" );
    ("-bs-v", string_call ignore, "*internal* version check to force a rebuild");
    ( "-bs-package-name",
      string_call Js_packages_state.set_package_name,
      "*internal* Set package name, useful when you want to produce npm \
       packages" );
    ( "-bs-ns",
      string_call Js_packages_state.set_package_map,
      "*internal* Set package map, not only set package name but also use it \
       as a namespace" );
    ( "-as-pp",
      unit_call (fun _ ->
          Js_config.as_pp := true;
          Js_config.syntax_only := true),
      "*internal*As pp to interact with native tools" );
    ( "-no-alias-deps",
      set Clflags.transparent_modules,
      "*internal*Do not record dependencies for module aliases" );
    ("-bs-gentype", set Clflags.bs_gentype, "*internal* Pass gentype command");
    (******************************************************************************)
    ( "-unboxed-types",
      set Clflags.unboxed_types,
      "*internal* Unannotated unboxable types will be unboxed" );
    ( "-bs-D",
      string_call define_variable,
      "Define conditional variable e.g, -D DEBUG=true" );
    ( "-bs-unsafe-empty-array",
      set Config.unsafe_empty_array,
      "*internal* Allow [||] to be polymorphic" );
    ("-nostdlib", set Js_config.no_stdlib, "*internal* Don't use stdlib");
    ( "-color",
      string_call set_color_option,
      "*internal* Enable or disable colors in compiler messages\n\
       The following settings are supported:\n\
       auto    use heuristics to enable colors only if supported\n\
       always  enable colors\n\
       never   disable colors\n\
       The default setting is 'always'\n\
       The current heuristic for 'auto'\n\
       checks that the TERM environment variable exists and is\n\
       not empty or \"dumb\", and that isatty(stderr) holds." );
    ( "-bs-list-conditionals",
      unit_call (fun () -> Pp.list_variables Format.err_formatter),
      "*internal* List existing conditional variables" );
    ( "-e",
      string_call (fun s -> eval s ~suffix:Literals.suffix_res),
      "(experimental) set the string to be evaluated in ReScript syntax" );
    ( "-bs-cmi-only",
      set Js_config.cmi_only,
      "*internal* Stop after generating cmi file" );
    ( "-bs-cmi",
      set Js_config.force_cmi,
      "*internal*  Not using cached cmi, always generate cmi" );
    ( "-bs-cmj",
      set Js_config.force_cmj,
      "*internal*  Not using cached cmj, always generate cmj" );
    ( "-bs-no-version-header",
      set Js_config.no_version_header,
      "*internal*Don't print version header" );
    ( "-bs-no-builtin-ppx",
      set Js_config.no_builtin_ppx,
      "*internal* Disable built-in ppx" );
    ( "-bs-cross-module-opt",
      set Js_config.cross_module_inline,
      "*internal* Enable cross module inlining(experimental), default(false)" );
    ( "-bs-no-cross-module-opt",
      clear Js_config.cross_module_inline,
      "*internal* Disable cross module inlining(experimental)" );
    ("-bs-diagnose", set Js_config.diagnose, "*internal* More verbose output");
    ( "-bs-no-check-div-by-zero",
      clear Js_config.check_div_by_zero,
      "*internal* unsafe mode, don't check div by zero and mod by zero" );
    ( "-bs-noassertfalse",
      set Clflags.no_assert_false,
      "*internal*  no code for assert false" );
    ( "-noassert",
      set Clflags.noassert,
      "*internal* Do not compile assertion checks" );
    ( "-bs-loc",
      set Clflags.dump_location,
      "*internal*  dont display location with -dtypedtree, -dparsetree" );
    ("-dtypedtree", set Clflags.dump_typedtree, "*internal* debug typedtree");
    ("-dparsetree", set Clflags.dump_parsetree, "*internal* debug parsetree");
    ("-drawlambda", set Clflags.dump_rawlambda, "*internal* debug raw lambda");
    ("-dsource", set Clflags.dump_source, "*internal* print source");
    ( "-reprint-source",
      string_call reprint_source_file,
      "*internal* transform the target ReScript file using PPXes provided, and \
       print the transformed ReScript code to stdout" );
    ("-format", string_call format_file, "*internal* Format as Res syntax");
    ("-only-parse", set Clflags.only_parse, "*internal* stop after parsing");
    ( "-ignore-parse-errors",
      set Clflags.ignore_parse_errors,
      "*internal* continue after parse errors" );
    ( "-where",
      unit_call print_standard_library,
      "*internal* Print location of standard library and exit" );
    ( "-verbose",
      set Clflags.verbose,
      "*internal* Print calls to external commands" );
    ( "-keep-locs",
      set Clflags.keep_locs,
      "*internal* Keep locations in .cmi files" );
    ( "-no-keep-locs",
      clear Clflags.keep_locs,
      "*internal* Do not keep locations in .cmi files" );
    ("-nopervasives", set Clflags.nopervasives, "*internal*");
    ("-uncurried", unit_call (fun () -> ()), "*internal* deprecated");
    ( "-v",
      unit_call print_version_string,
      "Print compiler version and location of standard library and exit" );
    ("-version", unit_call print_version_string, "Print version and exit");
    ( "-pp",
      string_optional_set Clflags.preprocessor,
      "*internal* <command>  Pipe sources through preprocessor <command>" );
    ( "-absname",
      set absname,
      "*internal* Show absolute filenames in error messages" );
    (* Not used, the build system did the expansion *)
    ( "-bs-no-bin-annot",
      clear Clflags.binary_annotations,
      "*internal* Disable binary annotations (by default on)" );
    ( "-short-paths",
      clear Clflags.real_paths,
      "*internal* Shorten paths in types" );
    ( "-unsafe",
      set Clflags.fast,
      "*internal* Do not compile bounds checking on array and string access" );
    ( "-warn-help",
      unit_call Warnings.help_warnings,
      "Show description of warning numbers" );
    ( "-warn-error",
      string_call (Warnings.parse_options true),
      "<list>  Enable or disable error status for warnings according\n\
       to <list>.  See option -w for the syntax of <list>.\n\
       Default setting is " ^ Bsc_warnings.defaults_warn_error );
    ( "-runtime",
      string_call setup_runtime_path,
      "*internal* Set the runtime directory" );
    ( "-make-runtime",
      unit_call Js_packages_state.make_runtime,
      "*internal* make runtime library" );
  |]

(** parse flags in config *)
let file_level_flags_handler (e : Parsetree.expression option) =
  match e with
  | None -> ()
  | Some {pexp_desc = Pexp_array args; pexp_loc} -> (
    let args =
      Array.of_list
        (Ext_list.map args (fun e ->
             match e.pexp_desc with
             | Pexp_constant (Pconst_string (name, _)) -> name
             | _ ->
               Location.raise_errorf ~loc:e.pexp_loc "string literal expected"))
    in
    try
      Bsc_args.parse_exn ~start:0 ~argv:args buckle_script_flags
        (fun ~rev_args:_ -> ())
        ~usage
    with _ ->
      Location.prerr_warning pexp_loc (Preprocessor "invalid flags for bsc"))
  | Some e -> Location.raise_errorf ~loc:e.pexp_loc "string array expected"

let _ : unit =
  Bs_conditional_initial.setup_env ();
  Clflags.color := Some Always;

  let flags = "flags" in
  Ast_config.add_structure flags file_level_flags_handler;
  Ast_config.add_signature flags file_level_flags_handler;
  try
    Bsc_args.parse_exn ~argv:Sys.argv buckle_script_flags anonymous ~usage
  with
  | Bsc_args.Bad msg ->
    Format.eprintf "%s@." msg;
    exit 2
  | x ->
    (*
        Ext_obj.bt ();
*)
    Location.report_exception ppf x;
    exit 2
