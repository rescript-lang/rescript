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

let absname_key = Domain.DLS.new_key (fun () -> ref false)
let absname () = Domain.DLS.get absname_key
exception Request_exit of int

module Error_message_utils_support = struct
  external to_comment : Res_comment.t -> Error_message_utils.Parser.comment
    = "%identity"
  external from_comment : Error_message_utils.Parser.comment -> Res_comment.t
    = "%identity"

  let setup () =
    (Error_message_utils.Parser.parse_source :=
       fun source ->
         let res =
           Res_driver.parse_implementation_from_source
             ~display_filename:"<none>" ~source
         in
         (res.parsetree, res.comments |> List.map to_comment));

    (Error_message_utils.Parser.reprint_source :=
       fun parsetree comments ->
         Res_printer.print_implementation parsetree
           ~comments:(comments |> List.map from_comment)
           ~width:80);

    Error_message_utils.configured_jsx_module ()
    := Some
         (match !((Js_config.current ()).jsx_module) with
         | React -> "React"
         | Generic {module_name} -> module_name)
end

let set_abs_input_name sourcefile =
  let sourcefile =
    if !(absname ()) && Filename.is_relative sourcefile then
      Ext_path.absolute_cwd_path sourcefile
    else sourcefile
  in
  Location.set_input_name sourcefile;
  sourcefile
let setup_outcome_printer () = Lazy.force Res_outcome_printer.setup

let setup_runtime_path path = Runtime_package.set_path path

let process_file sourcefile ?kind ppf =
  Compiler_phase_trace.section "request.other" (fun () ->
      (* The input name must identify the source when writing the binary AST. *)
      setup_outcome_printer ();
      Error_message_utils_support.setup ();
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
                 ~ignore_parse_errors:
                   !((Clflags.current ()).ignore_parse_errors))
            ppf sourcefile
        | Resi ->
          let sourcefile = set_abs_input_name sourcefile in
          Js_implementation.interface
            ~parser:
              (Res_driver.parse_interface
                 ~ignore_parse_errors:
                   !((Clflags.current ()).ignore_parse_errors))
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
          let output = Compiler_request_output.stdout_formatter () in
          Printtyp.signature output cmi_sign;
          Format.pp_print_newline output ()
        | Unknown ->
          Bsc_args.bad_arg ("don't know what to do with " ^ sourcefile)
      in
      res)

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
        Res_driver.parsing_engine.parse_implementation ~filename:sourcefile
      in
      if parse_result.invalid then (
        Res_diagnostics.print_report parse_result.diagnostics
          parse_result.source;
        raise_notrace (Request_exit 1));
      Res_compmisc.init_path ();
      parse_result.parsetree
      |> Cmd_ppx_apply.apply_rewriters ~restore:false
           ~tool_name:Js_config.tool_name Ml
      |> Ppx_entry.rewrite_implementation
      |> Res_printer.print_implementation ~width:100
           ~comments:parse_result.comments
      |> Compiler_request_output.print_stdout
    | Resi ->
      let parse_result =
        Res_driver.parsing_engine.parse_interface ~filename:sourcefile
      in
      if parse_result.invalid then (
        Res_diagnostics.print_report parse_result.diagnostics
          parse_result.source;
        raise_notrace (Request_exit 1));
      Res_compmisc.init_path ();
      parse_result.parsetree
      |> Cmd_ppx_apply.apply_rewriters ~restore:false
           ~tool_name:Js_config.tool_name Mli
      |> Ppx_entry.rewrite_signature
      |> Res_printer.print_interface ~width:100 ~comments:parse_result.comments
      |> Compiler_request_output.print_stdout
    | _ ->
      Compiler_request_output.print_stdout
        ("Invalid input for reprinting ReScript source. Must be a ReScript \
          file: " ^ sourcefile);
      raise_notrace (Request_exit 2)
  in
  res

let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf () = Compiler_request_output.stderr_formatter ()

(* Error messages to standard error formatter *)

let anonymous ~(rev_args : string list) =
  match rev_args with
  | [filename] -> process_file filename (ppf ())
  | [] -> ()
  | _ ->
    if !((Js_config.current ()).syntax_only) then
      Ext_list.rev_iter rev_args (fun filename ->
          Clflags.reset_dump_state ();
          Warnings.reset ();
          process_file filename (ppf ()))
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
      ~ignore_parse_errors:!((Clflags.current ()).ignore_parse_errors)
      input
  in
  match !((Clflags.current ()).output_name) with
  | None -> Compiler_request_output.write_stdout formatted
  | Some fname -> Ext_io.write_file fname formatted

let set_color_option option =
  match Clflags.parse_color_setting option with
  | None -> ()
  | Some setting -> (Clflags.current ()).color := Some setting

let eval (s : string) ~suffix =
  let tmpfile = Filename.temp_file "eval" suffix in
  Ext_io.write_file tmpfile s;
  anonymous ~rev_args:[tmpfile];
  if not !((Clflags.current ()).verbose) then
    try Sys.remove tmpfile with _ -> ()

(* let (//) = Filename.concat *)

let bs_version_string = "ReScript " ^ Bs_version.version

let print_version_string () =
  Compiler_request_output.print_stdout bs_version_string;
  raise_notrace (Request_exit 0)

let[@inline] set s : Bsc_args.spec = Unit (Unit_set s)
let[@inline] clear s : Bsc_args.spec = Unit (Unit_clear s)
let[@inline] string_call s : Bsc_args.spec = String (String_call s)
let[@inline] string_optional_set s : Bsc_args.spec =
  String (String_optional_set s)

let[@inline] unit_call s : Bsc_args.spec = Unit (Unit_call s)
let[@inline] string_list_add s : Bsc_args.spec = String (String_list_add s)

let parse_source_map value =
  (Js_config.current ()).source_map :=
    match String.lowercase_ascii value with
    | "linked" -> Linked
    | "inline" -> Inline
    | "hidden" -> Hidden
    | "false" | "none" -> No_source_map
    | value ->
      Bsc_args.bad_arg
        ("Unsupported sourceMap value: " ^ value
       ^ ". Expected linked, inline, hidden, false, or none")

let parse_bool_ref target value =
  target :=
    match String.lowercase_ascii value with
    | "true" -> true
    | "false" -> false
    | value -> Bsc_args.bad_arg ("Expected true or false, got: " ^ value)

(* mostly common used to list in the beginning to make search fast
*)
let command_line_flags () : (string * Bsc_args.spec * string) array =
  [|
    ( "-I",
      string_list_add (Clflags.current ()).include_dirs,
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
      string_optional_set (Clflags.current ()).output_name,
      "*internal* <file>  set output file name to <file>" );
    ( "-bs-read-cmi",
      unit_call (fun _ -> (Clflags.current ()).assume_no_mli := Mli_exists),
      "*internal* Assume mli always exist " );
    ( "-ppx",
      string_list_add (Clflags.current ()).all_ppx,
      "*internal* <command>  Pipe abstract syntax trees through preprocessor \
       <command>" );
    ( "-open",
      string_list_add (Clflags.current ()).open_modules,
      "*internal* <module>  Opens the module <module> before typing" );
    ( "-bs-jsx",
      string_call (fun i ->
          if i <> "4" then Bsc_args.bad_arg ("Unsupported jsx version: " ^ i);
          (Js_config.current ()).jsx_version :=
            Js_config.jsx_version_of_int @@ int_of_string i),
      "*internal* Set jsx version" );
    ( "-bs-jsx-module",
      string_call (fun i ->
          let is_generic =
            match i |> String.lowercase_ascii with
            | "react" -> false
            | _ -> true
          in
          (Js_config.current ()).jsx_module := Js_config.jsx_module_of_string i;
          if is_generic then (Js_config.current ()).jsx_version := Some Jsx_v4),
      "*internal* Set jsx module" );
    ( "-bs-jsx-mode",
      string_call ignore,
      "*internal* Set jsx mode, this is no longer used and is a no-op." );
    ( "-bs-jsx-preserve",
      set (Js_config.current ()).jsx_preserve,
      "*internal* Preserve jsx" );
    ( "-bs-source-map",
      string_call parse_source_map,
      "*internal* Configure source map output" );
    ( "-bs-source-map-sources-content",
      string_call
        (parse_bool_ref (Js_config.current ()).source_map_sources_content),
      "*internal* Include original source text in source maps" );
    ( "-bs-source-map-root",
      string_call (fun value -> (Js_config.current ()).source_map_root := value),
      "*internal* Set sourceRoot in source maps" );
    ( "-bs-package-output",
      string_call Js_packages_state.update_npm_package_path,
      "*internal* Set npm-output-path: [opt_module]:path, for example: \
       'lib/cjs', 'amdjs:lib/amdjs', 'es6:lib/es6' " );
    ( "-bs-project-root",
      string_call (fun s ->
          Ext_path.set_project_root s;
          Gentype_config.project_root () := s),
      "*internal* Set the project root directory" );
    ( "-bs-ast",
      unit_call (fun _ ->
          (Js_config.current ()).binary_ast := true;
          (Js_config.current ()).syntax_only := true),
      "*internal* Generate binary .mli_ast and ml_ast and stop" );
    ( "-bs-test-ast-conversion",
      set (Js_config.current ()).test_ast_conversion,
      "*internal* Roundtrip the parsed AST through Parsetree0 before continuing"
    );
    ( "-bs-syntax-only",
      set (Js_config.current ()).syntax_only,
      "*internal* Only check syntax" );
    ("-bs-g", set (Js_config.current ()).debug, "Debug mode");
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
          (Js_config.current ()).as_pp := true;
          (Js_config.current ()).syntax_only := true),
      "*internal*As pp to interact with native tools" );
    ( "-no-alias-deps",
      set (Clflags.current ()).transparent_modules,
      "*internal*Do not record dependencies for module aliases" );
    ( "-bs-gentype",
      set (Clflags.current ()).bs_gentype,
      "*internal* Pass gentype command" );
    ( "-bs-gentype-module",
      string_call (fun s ->
          Gentype_config.module_flag () := Gentype_config.module_of_string s),
      "*internal* Set gentype module system: commonjs|esmodule" );
    ( "-bs-gentype-module-resolution",
      string_call (fun s ->
          Gentype_config.module_resolution_flag ()
          := Gentype_config.module_resolution_of_string s),
      "*internal* Set gentype module resolution strategy: node|node16|bundler"
    );
    ( "-bs-gentype-export-interfaces",
      set (Gentype_config.export_interfaces_flag ()),
      "*internal* Emit gentype interface files" );
    ( "-bs-gentype-generated-extension",
      string_call (fun s ->
          Gentype_config.generated_file_extension_flag () := Some s),
      "*internal* Set gentype generated-file extension (e.g. .gen.tsx)" );
    ( "-bs-gentype-suffix",
      string_call (fun s -> Gentype_config.suffix_flag () := Some s),
      "*internal* Set gentype import-path suffix (e.g. .bs.js, .mjs)" );
    ( "-bs-gentype-shim",
      string_call Gentype_config.add_shim,
      "*internal* Register a gentype shim mapping: From=To (repeatable)" );
    ( "-bs-gentype-debug",
      string_call Debug.set_item,
      "*internal* Enable a gentype debug category (repeatable): \
       all|basic|codeItems|config|converter|dependencies|moduleResolution|notImplemented|translation|typeEnv|typeResolution"
    );
    ( "-bs-gentype-dep",
      string_call Gentype_config.add_bs_dependency,
      "*internal* Register a gentype bsb dependency (repeatable)" );
    ( "-bs-gentype-source-dir",
      string_call Gentype_config.add_source_dir,
      "*internal* Register a gentype source directory relative to the project \
       root (repeatable)" );
    ( "-bs-gentype-dep-path",
      string_call Gentype_config.add_dep_path,
      "*internal* Register a gentype dependency install path: \
       <name>=<absolute-path> (repeatable)" );
    ( "-bs-gentype-bsb-project-root",
      string_call (fun s -> Gentype_config.bsb_project_root () := s),
      "*internal* Set gentype bsb project root (workspace root containing \
       .sourcedirs.json)" );
    (******************************************************************************)
    ( "-nostdlib",
      set (Js_config.current ()).no_stdlib,
      "*internal* Don't use stdlib" );
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
    ( "-e",
      string_call (fun s -> eval s ~suffix:Literals.suffix_res),
      "(experimental) set the string to be evaluated in ReScript syntax" );
    ( "-bs-cmi-only",
      set (Js_config.current ()).cmi_only,
      "*internal* Stop after generating cmi file" );
    ( "-bs-cmi",
      set (Js_config.current ()).force_cmi,
      "*internal*  Not using cached cmi, always generate cmi" );
    ( "-bs-cmj",
      set (Js_config.current ()).force_cmj,
      "*internal*  Not using cached cmj, always generate cmj" );
    ( "-bs-no-version-header",
      set (Js_config.current ()).no_version_header,
      "*internal*Don't print version header" );
    ( "-bs-no-builtin-ppx",
      set (Js_config.current ()).no_builtin_ppx,
      "*internal* Disable built-in ppx" );
    ( "-bs-cross-module-opt",
      set (Js_config.current ()).cross_module_inline,
      "*internal* Enable cross module inlining(experimental), default(false)" );
    ( "-bs-no-cross-module-opt",
      clear (Js_config.current ()).cross_module_inline,
      "*internal* Disable cross module inlining(experimental)" );
    ( "-debug-ir",
      set (Js_config.current ()).debug_ir,
      "*internal* Dump compiler IR and enable Lambda invariant checks" );
    ( "-check-lam",
      set (Js_config.current ()).check_lam,
      "*internal* Check Lambda invariants after optimization passes" );
    ( "-bs-no-check-div-by-zero",
      clear (Js_config.current ()).check_div_by_zero,
      "*internal* unsafe mode, don't check div by zero and mod by zero" );
    ( "-bs-noassertfalse",
      set (Clflags.current ()).no_assert_false,
      "*internal*  no code for assert false" );
    ( "-noassert",
      set (Clflags.current ()).noassert,
      "*internal* Do not compile assertion checks" );
    ( "-bs-loc",
      set (Clflags.current ()).dump_location,
      "*internal*  dont display location with -dtypedtree, -dparsetree" );
    ( "-dtypedtree",
      set (Clflags.current ()).dump_typedtree,
      "*internal* debug typedtree" );
    ( "-dparsetree",
      set (Clflags.current ()).dump_parsetree,
      "*internal* debug parsetree" );
    ( "-drawlambda",
      set (Clflags.current ()).dump_rawlambda,
      "*internal* debug raw lambda" );
    ("-dsource", set (Clflags.current ()).dump_source, "*internal* print source");
    ( "-reprint-source",
      string_call reprint_source_file,
      "*internal* transform the target ReScript file using PPXes provided, and \
       print the transformed ReScript code to stdout" );
    ("-format", string_call format_file, "*internal* Format as Res syntax");
    ( "-only-parse",
      set (Clflags.current ()).only_parse,
      "*internal* stop after parsing" );
    ( "-editor-mode",
      unit_call (fun () ->
          (Clflags.current ()).editor_mode := true;
          (Clflags.current ()).ignore_parse_errors := true;
          (Js_config.current ()).cmi_only := true),
      "*internal* Enable editor mode." );
    ( "-ignore-parse-errors",
      set (Clflags.current ()).ignore_parse_errors,
      "*internal* continue after parse errors" );
    ( "-verbose",
      set (Clflags.current ()).verbose,
      "*internal* Print calls to external commands" );
    ( "-keep-locs",
      set (Clflags.current ()).keep_locs,
      "*internal* Keep locations in .cmi files" );
    ( "-no-keep-locs",
      clear (Clflags.current ()).keep_locs,
      "*internal* Do not keep locations in .cmi files" );
    ("-nopervasives", set (Clflags.current ()).nopervasives, "*internal*");
    ( "-v",
      unit_call print_version_string,
      "Print compiler version and location of standard library and exit" );
    ("-version", unit_call print_version_string, "Print version and exit");
    ( "-pp",
      string_optional_set (Clflags.current ()).preprocessor,
      "*internal* <command>  Pipe sources through preprocessor <command>" );
    ( "-absname",
      set (absname ()),
      "*internal* Show absolute filenames in error messages" );
    ( "-enable-experimental",
      string_call Experimental_features.enable_from_string,
      "Enable experimental features: repeatable, e.g. -enable-experimental \
       LetUnwrap" );
    (* Not used, the build system did the expansion *)
    ( "-bs-no-bin-annot",
      clear (Clflags.current ()).binary_annotations,
      "*internal* Disable binary annotations (by default on)" );
    ( "-short-paths",
      clear (Clflags.current ()).real_paths,
      "*internal* Shorten paths in types" );
    ( "-unsafe",
      set (Clflags.current ()).fast,
      "*internal* Do not compile bounds checking on array and string access" );
    ( "-runtime-path",
      string_call setup_runtime_path,
      "*internal* Set the path of the runtime package (@rescript/runtime)" );
    ( "-warn-help",
      unit_call (fun () ->
          Warnings.help_warnings ();
          raise_notrace (Request_exit 0)),
      "Show description of warning numbers" );
    ( "-warn-error",
      string_call (Warnings.parse_options true),
      "<list>  Enable or disable error status for warnings according\n\
       to <list>.  See option -w for the syntax of <list>.\n\
       Default setting is " ^ Bsc_warnings.defaults_warn_error );
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
             match Ast_payload.semantic_string_of_expression e with
             | Some name -> name
             | _ ->
               Location.raise_errorf ~loc:e.pexp_loc "string literal expected"))
    in
    try
      Bsc_args.parse_exn ~start:0 ~argv:args (command_line_flags ())
        (fun ~rev_args:_ -> ())
        ~usage
    with _ ->
      Location.prerr_warning pexp_loc (Preprocessor "invalid flags for bsc"))
  | Some e -> Location.raise_errorf ~loc:e.pexp_loc "string array expected"

(* These shared hooks are installed when the driver module loads, before any
   request can start on another domain. Concurrent Lazy.force calls would race
   on a first request. *)
let () =
  Bs_conditional_initial.setup_env ();
  setup_outcome_printer ();
  (Clflags.current ()).color := Some Always;
  let flags = "flags" in
  Ast_config.add_structure flags file_level_flags_handler;
  Ast_config.add_signature flags file_level_flags_handler;
  Ident.capture_request_baseline ()

type result = {
  exit_code: int;
  stdout: string;
  stderr: string;
  diagnostics: Location.diagnostic list;
}
type published_cmj = {
  filename: string;
  source: string option;
  stats: Unix.stats;
  fingerprint: Digest.t;
  image: Js_cmj_format.frozen;
}
type published_ast = {stats: Unix.stats; result: Binary_ast.result}
type published_semantic = {
  stats: Unix.stats;
  generation: int;
  value: Cmt_format.cmt_infos;
}
type published_fingerprint = {
  source: string option;
  stats: Unix.stats;
  fingerprint: Digest.t;
}
type 'a staged_artifact = {stats: Unix.stats; value: 'a}
type staged_semantic = {
  stats: Unix.stats;
  value: Cmt_format.cmt_infos;
  generation: int;
}
type module_result = {
  interface_file: string;
  interface_source: string option;
  interface_stats: Unix.stats;
  interface_fingerprint: Digest.t option;
  interface_image: Frozen_values.t option;
  optimization:
    (string
    * string option
    * Unix.stats
    * Digest.t
    * Js_cmj_format.frozen option)
    option;
  semantic: unit -> Cmt_format.cmt_infos option;
  diagnostics: Location.diagnostic list;
  dependencies: string list;
  generated_outputs: string list;
}
type session = {
  dependencies: Env.dependency_cache;
  frozen_enabled: bool Atomic.t;
  use_frozen_for_compile: bool Atomic.t;
  staging_lock: Mutex.t;
  staged_cmis:
    (string, (Digest.t * Cmi_format.cmi_infos) staged_artifact) Hashtbl.t;
  staged_cmjs: (string, (Digest.t * Js_cmj_format.t) staged_artifact) Hashtbl.t;
  published_cmjs: (string, published_cmj) Hashtbl.t;
  cmi_fingerprints: (string, published_fingerprint) Hashtbl.t;
  cmj_fingerprints: (string, published_fingerprint) Hashtbl.t;
  staged_diagnostics: (string, Location.diagnostic list) Hashtbl.t;
  staged_generated_outputs: (string, string list) Hashtbl.t;
  staged_request_files: (string, string list) Hashtbl.t;
  request_generations: (string, int) Hashtbl.t;
  mutable next_request_generation: int;
  published_results: (string, module_result) Hashtbl.t;
  staged_asts: (string, Binary_ast.result staged_artifact) Hashtbl.t;
  published_asts: (string, published_ast) Hashtbl.t;
  staged_semantics: (string, staged_semantic) Hashtbl.t;
  mutable staged_semantic_bytes: int;
  mutable staged_semantic_generation: int;
  published_semantics: (string, published_semantic) Hashtbl.t;
  mutable semantic_bytes: int;
  mutable semantic_generation: int;
}

let create_session () =
  {
    dependencies = Env.create_dependency_cache ();
    frozen_enabled = Atomic.make true;
    use_frozen_for_compile = Atomic.make true;
    staging_lock = Mutex.create ();
    staged_cmis = Hashtbl.create 32;
    staged_cmjs = Hashtbl.create 32;
    published_cmjs = Hashtbl.create 64;
    cmi_fingerprints = Hashtbl.create 64;
    cmj_fingerprints = Hashtbl.create 64;
    staged_diagnostics = Hashtbl.create 64;
    staged_generated_outputs = Hashtbl.create 64;
    staged_request_files = Hashtbl.create 64;
    request_generations = Hashtbl.create 64;
    next_request_generation = 0;
    published_results = Hashtbl.create 64;
    staged_asts = Hashtbl.create 64;
    published_asts = Hashtbl.create 64;
    staged_semantics = Hashtbl.create 32;
    staged_semantic_bytes = 0;
    staged_semantic_generation = 0;
    published_semantics = Hashtbl.create 32;
    semantic_bytes = 0;
    semantic_generation = 0;
  }

let set_frozen_for_compile session enabled =
  Atomic.set session.use_frozen_for_compile enabled

let set_session_frozen_enabled session enabled =
  Atomic.set session.frozen_enabled enabled

let session_frozen_enabled session =
  Sys.getenv_opt "REWATCH_FROZEN_VALUES" <> Some "0"
  && Atomic.get session.frozen_enabled

let same_file_stats first second =
  first.Unix.st_dev = second.Unix.st_dev
  && first.Unix.st_ino = second.Unix.st_ino
  && first.Unix.st_size = second.Unix.st_size
  && first.Unix.st_mtime = second.Unix.st_mtime
  && first.Unix.st_ctime = second.Unix.st_ctime

let unit_name_of_artifact filename =
  filename |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let lookup_session_cmj session name filename =
  let entry =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () -> Hashtbl.find_opt session.published_cmjs name)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  match entry with
  | None -> None
  | Some entry -> (
    try
      let selected = Compiler_request_state.canonical_output_path filename in
      let checked = Option.value entry.source ~default:entry.filename in
      if
        Compiler_request_state.same_output_path selected entry.filename
        && same_file_stats (Unix.stat checked) entry.stats
      then Some (Js_cmj_format.view entry.image)
      else None
    with Sys_error _ | Unix.Unix_error _ -> None)

let staged_ast_dependencies session ~path =
  Mutex.lock session.staging_lock;
  Fun.protect
    (fun () ->
      Hashtbl.find_opt session.staged_asts path
      |> Option.map (fun (staged : Binary_ast.result staged_artifact) ->
          Binary_ast.dependencies staged.value))
    ~finally:(fun () -> Mutex.unlock session.staging_lock)

let take_session_ast session filename =
  let path = Compiler_request_state.resolve_path filename in
  let entry =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        let entry = Hashtbl.find_opt session.published_asts path in
        Hashtbl.remove session.published_asts path;
        entry)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  match entry with
  | None -> None
  | Some entry -> (
    try
      if same_file_stats (Unix.stat path) entry.stats then Some entry.result
      else None
    with Sys_error _ | Unix.Unix_error _ -> None)

let build_identity = Rescript_compiler_build_identity.value

let reset_state ?(new_request = false) () =
  Clflags.reset ();
  Js_config.reset ();
  Js_packages_state.reset ();
  Gentype_config.reset_flags ();
  Debug.reset ();
  Experimental_features.reset ();
  Error_message_utils.configured_jsx_module () := None;
  Warnings.reset ();
  Warnings.reset_fatal ();
  Warnings.reset_has_warnings ();
  Runtime_package.reset_path ();
  Ext_path.reset_project_root ();
  Config.set_load_path [];
  Env.reset_cache ();
  Printtyp.reset_request ();
  Delayed_checks.reset_delayed_checks ();
  Bs_builtin_ppx.reset ();
  Used_attributes.reset ();
  Btype.reinit ();
  if new_request then (
    (* Allocate shared predefined graphs in a fixed order. Lazy first use on
       different worker domains would otherwise shift serialized type IDs. *)
    Predef.reset_for_request ();
    Env.reset_initial_for_request ());
  Lambda_exits.reset ();
  Lam_compile_env.reset ();
  Cmt_format.clear ();
  Location.reset_input_name ();
  absname () := false

let with_fresh_request_states ~cwd action =
  Predef.with_fresh (fun () ->
      Env.with_fresh_initial (fun () ->
          Lam_compile_env.with_fresh (fun () ->
              Typecore.with_fresh (fun () ->
                  Stypes.with_fresh (fun () ->
                      Ident.with_fresh (fun () ->
                          Clflags.with_fresh (fun () ->
                              Env.with_fresh (fun () ->
                                  Js_config.with_fresh (fun () ->
                                      Warnings.with_fresh (fun () ->
                                          Experimental_features.with_fresh
                                            (fun () ->
                                              Btype.with_fresh (fun () ->
                                                  Js_packages_state.with_fresh
                                                    (fun () ->
                                                      Compiler_request_state
                                                      .with_fresh ~cwd action)))))))))))))

let with_fresh_request_states_and_snapshot ~cwd action =
  Fun.protect
    (fun () -> with_fresh_request_states ~cwd action)
    ~finally:Env.finalize_expanded_snapshot_cache

let run_argv ?run_external ?frozen_override ~cwd argv =
  let input = argv.(Array.length argv - 1) in
  Env.with_frozen_values_setting ?enabled:frozen_override (fun () ->
      Compiler_phase_trace.request ~cwd ~input (fun () ->
          with_fresh_request_states_and_snapshot ~cwd (fun () ->
              Compiler_phase_trace.section "request.reset" (fun () ->
                  reset_state ~new_request:true ());
              Cmt_format.set_args argv;
              let execute () =
                try
                  let flags =
                    Compiler_phase_trace.section "request.flags"
                      command_line_flags
                  in
                  Compiler_phase_trace.section "request.dispatch" (fun () ->
                      Bsc_args.parse_exn ~argv flags anonymous ~usage);
                  0
                with
                | Request_exit code -> code
                | Bsc_args.Help message ->
                  Compiler_request_output.write_stdout message;
                  0
                | Res_driver.Already_reported -> 1
                | Bsc_args.Bad msg ->
                  Format.fprintf (ppf ()) "%s@." msg;
                  2
                | x ->
                  Location.report_exception (ppf ()) x;
                  2
              in
              let run_with_external_owner action =
                match run_external with
                | None -> action ()
                | Some run_external ->
                  Ccomp.with_command_runner
                    (fun command ->
                      let status, stdout, stderr = run_external command in
                      Compiler_request_output.write_stdout stdout;
                      Format.pp_print_string (ppf ()) stderr;
                      status)
                    action
              in
              let (exit_code, stdout, stderr), diagnostics =
                Fun.protect
                  (fun () ->
                    Location.with_diagnostic_capture (fun () ->
                        Compiler_request_output.with_capture (fun () ->
                            Misc.Color.set_color_tag_handling
                              (Compiler_request_output.stdout_formatter ());
                            Misc.Color.set_color_tag_handling
                              (Compiler_request_output.stderr_formatter ());
                            run_with_external_owner execute)))
                  ~finally:reset_state
              in
              {exit_code; stdout; stderr; diagnostics})))

let run_request ~run_external ~cwd ~argv ~input =
  let logical_argv = Array.of_list ("bsc" :: (argv @ [input])) in
  run_argv ?run_external ~cwd logical_argv

let run_request_with_frozen ~frozen_override ~run_external ~cwd ~argv ~input =
  let logical_argv = Array.of_list ("bsc" :: (argv @ [input])) in
  run_argv ?run_external ~frozen_override ~cwd logical_argv

let remove_staged_semantic session filename =
  match Hashtbl.find_opt session.staged_semantics filename with
  | None -> ()
  | Some entry ->
    session.staged_semantic_bytes <-
      session.staged_semantic_bytes - entry.stats.Unix.st_size;
    Hashtbl.remove session.staged_semantics filename

let stage_semantic session filename value =
  try
    let stats = Unix.stat filename in
    remove_staged_semantic session filename;
    if stats.Unix.st_size <= 4 * 1024 * 1024 then (
      session.staged_semantic_generation <-
        session.staged_semantic_generation + 1;
      Hashtbl.replace session.staged_semantics filename
        {stats; value; generation = session.staged_semantic_generation};
      session.staged_semantic_bytes <-
        session.staged_semantic_bytes + stats.Unix.st_size;
      while
        session.staged_semantic_bytes > 16 * 1024 * 1024
        || Hashtbl.length session.staged_semantics > 64
      do
        let oldest =
          Hashtbl.fold
            (fun path (entry : staged_semantic) oldest ->
              match oldest with
              | Some (_, generation) when generation <= entry.generation ->
                oldest
              | _ -> Some (path, entry.generation))
            session.staged_semantics None
        in
        match oldest with
        | None -> assert false
        | Some (path, _) -> remove_staged_semantic session path
      done;
      true)
    else false
  with Sys_error _ | Unix.Unix_error _ -> false

let run_request_in_session session ~run_external ~cwd ~argv ~input =
  let input_path =
    if Filename.is_relative input then Filename.concat cwd input else input
  in
  let generation =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        List.iter
          (fun path ->
            Hashtbl.remove session.staged_cmis path;
            Hashtbl.remove session.staged_cmjs path;
            Hashtbl.remove session.staged_asts path;
            remove_staged_semantic session path)
          (Hashtbl.find_opt session.staged_request_files input_path
          |> Option.value ~default:[]);
        Hashtbl.remove session.staged_request_files input_path;
        Hashtbl.remove session.staged_diagnostics input_path;
        Hashtbl.remove session.staged_generated_outputs input_path;
        session.next_request_generation <- session.next_request_generation + 1;
        let generation = session.next_request_generation in
        Hashtbl.replace session.request_generations input_path generation;
        generation)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  Env.with_dependency_cache session.dependencies (fun () ->
      let frozen_enabled = session_frozen_enabled session in
      let cmi_enabled =
        frozen_enabled && Sys.getenv_opt "REWATCH_SESSION_CMI" <> Some "0"
      in
      let cmj_enabled =
        frozen_enabled && Sys.getenv_opt "REWATCH_SESSION_CMJ" <> Some "0"
      in
      let ast_enabled =
        frozen_enabled && Sys.getenv_opt "REWATCH_SESSION_AST" <> Some "0"
      in
      let use_session_cmj_lookup =
        cmj_enabled && Atomic.get session.use_frozen_for_compile
      in
      let compiled_cmi = ref None in
      let compiled_cmj = ref None in
      let parsed_ast = ref None in
      let semantic = ref None in
      let generated_outputs = ref [] in
      let run () =
        let use_frozen =
          frozen_enabled
          && (List.mem "-bs-ast" argv
             || Atomic.get session.use_frozen_for_compile)
        in
        run_request_with_frozen ~frozen_override:use_frozen ~run_external ~cwd
          ~argv ~input
      in
      let run () =
        if cmi_enabled then
          Env.with_compiled_cmi_capture
            (fun filename crc cmi ->
              compiled_cmi :=
                Some (Compiler_request_state.resolve_path filename, crc, cmi))
            run
        else run ()
      in
      let run () =
        if cmj_enabled then
          Js_cmj_format.with_capture
            (fun filename fingerprint cmj ->
              compiled_cmj :=
                Some
                  ( Compiler_request_state.resolve_path filename,
                    fingerprint,
                    cmj ))
            run
        else run ()
      in
      let run () =
        if ast_enabled then
          Binary_ast.with_capture
            (fun filename ast ->
              parsed_ast :=
                Some (Compiler_request_state.resolve_path filename, ast))
            run
        else run ()
      in
      let run () =
        Cmt_format.with_capture
          (fun filename cmt ->
            semantic := Some (Compiler_request_state.resolve_path filename, cmt))
          run
      in
      let run () =
        Gentype_main.with_generated_output_capture
          (fun filename ->
            generated_outputs :=
              Compiler_request_state.resolve_path filename :: !generated_outputs)
          run
      in
      let result =
        let run () =
          if use_session_cmj_lookup then
            Js_cmj_load.with_session_lookup (lookup_session_cmj session) run
          else run ()
        in
        if ast_enabled then
          Binary_ast.with_lookup (take_session_ast session) run
        else run ()
      in
      (match result.exit_code with
      | code when code <> 0 -> ()
      | _ ->
        Mutex.lock session.staging_lock;
        Fun.protect
          (fun () ->
            if
              Hashtbl.find_opt session.request_generations input_path
              = Some generation
            then (
              let files = ref [] in
              let record filename = files := filename :: !files in
              let stage table filename value =
                try
                  let stats = Unix.stat filename in
                  record filename;
                  Hashtbl.replace table filename {stats; value}
                with Sys_error _ | Unix.Unix_error _ -> ()
              in
              if
                Option.is_some !compiled_cmi
                || Option.is_some !compiled_cmj
                || Option.is_some !semantic
              then
                Hashtbl.replace session.staged_diagnostics input_path
                  result.diagnostics;
              Hashtbl.replace session.staged_generated_outputs input_path
                !generated_outputs;
              Option.iter
                (fun (filename, crc, cmi) ->
                  stage session.staged_cmis filename (crc, cmi))
                !compiled_cmi;
              Option.iter
                (fun (filename, fingerprint, cmj) ->
                  stage session.staged_cmjs filename (fingerprint, cmj))
                !compiled_cmj;
              Option.iter
                (fun (filename, ast) -> stage session.staged_asts filename ast)
                !parsed_ast;
              Option.iter
                (fun (filename, cmt) ->
                  if stage_semantic session filename cmt then record filename)
                !semantic;
              Hashtbl.replace session.staged_request_files input_path !files))
          ~finally:(fun () -> Mutex.unlock session.staging_lock));
      result)

let validated_stage source (staged : 'a staged_artifact) =
  try
    if same_file_stats (Unix.stat source) staged.stats then Some staged.value
    else None
  with Sys_error _ | Unix.Unix_error _ -> None

let staged_value session table source =
  let staged =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () -> Hashtbl.find_opt table source)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  Option.bind staged (validated_stage source)

let stage_session_cmi session ~source ~destination =
  match staged_value session session.staged_cmis source with
  | None -> false
  | Some (crc, cmi) ->
    if
      Env.publish_pending_compiled_cmi session.dependencies ~source ~destination
        ~crc cmi
    then (
      Atomic.set session.use_frozen_for_compile true;
      let stats = Unix.stat source in
      Mutex.lock session.staging_lock;
      Fun.protect
        (fun () ->
          Hashtbl.replace session.cmi_fingerprints destination
            {source = Some source; stats; fingerprint = crc})
        ~finally:(fun () -> Mutex.unlock session.staging_lock);
      true)
    else false

let stage_session_cmj session ~source ~destination =
  match staged_value session session.staged_cmjs source with
  | None -> false
  | Some (fingerprint, cmj) ->
    Atomic.set session.use_frozen_for_compile true;
    let stats = Unix.stat source in
    let image = Js_cmj_format.freeze cmj in
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        Hashtbl.replace session.cmj_fingerprints destination
          {source = Some source; stats; fingerprint};
        let name = unit_name_of_artifact destination in
        Hashtbl.replace session.published_cmjs name
          {
            filename = Compiler_request_state.canonical_output_path destination;
            source = Some source;
            stats;
            fingerprint;
            image;
          })
      ~finally:(fun () -> Mutex.unlock session.staging_lock);
    true

let discard_pending_session_artifacts session ~interface_file ~optimization_file
    =
  Env.discard_pending_compiled_cmi session.dependencies ~filename:interface_file;
  Mutex.lock session.staging_lock;
  Fun.protect
    (fun () ->
      (match Hashtbl.find_opt session.published_results interface_file with
      | Some result when Option.is_some result.interface_source ->
        Hashtbl.remove session.published_results interface_file
      | Some _ | None -> ());
      let discard table filename =
        match Hashtbl.find_opt table filename with
        | Some {source = Some _; stats = _; fingerprint = _} ->
          Hashtbl.remove table filename
        | Some _ | None -> ()
      in
      discard session.cmi_fingerprints interface_file;
      Option.iter
        (fun filename ->
          discard session.cmj_fingerprints filename;
          let name = unit_name_of_artifact filename in
          match Hashtbl.find_opt session.published_cmjs name with
          | Some
              {
                filename = published;
                source = Some _;
                stats = _;
                fingerprint = _;
                image = _;
              }
            when Compiler_request_state.same_output_path published filename ->
            Hashtbl.remove session.published_cmjs name
          | Some _ | None -> ())
        optimization_file)
    ~finally:(fun () -> Mutex.unlock session.staging_lock)

let publish_session_cmi session ~retain ~source ~destination =
  let staged =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        let staged = Hashtbl.find_opt session.staged_cmis source in
        Hashtbl.remove session.staged_cmis source;
        staged)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  Option.bind staged (validated_stage source)
  |> Option.iter (fun (crc, cmi) ->
      let stats = Unix.stat destination in
      Mutex.lock session.staging_lock;
      Fun.protect
        (fun () ->
          Hashtbl.replace session.cmi_fingerprints destination
            {source = None; stats; fingerprint = crc})
        ~finally:(fun () -> Mutex.unlock session.staging_lock);
      if retain then
        Env.publish_compiled_cmi session.dependencies ~filename:destination ~crc
          cmi)

let publish_session_cmj session ~retain ~source ~destination =
  let staged =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        let staged = Hashtbl.find_opt session.staged_cmjs source in
        Hashtbl.remove session.staged_cmjs source;
        staged)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  Option.bind staged (validated_stage source)
  |> Option.iter (fun (fingerprint, cmj) ->
      let stats = Unix.stat destination in
      let name = unit_name_of_artifact destination in
      let pending_image =
        Mutex.lock session.staging_lock;
        Fun.protect
          (fun () ->
            match Hashtbl.find_opt session.published_cmjs name with
            | Some entry
              when entry.source = Some source
                   && entry.fingerprint = fingerprint
                   && Compiler_request_state.same_output_path entry.filename
                        destination ->
              Some entry.image
            | Some _ | None -> None)
          ~finally:(fun () -> Mutex.unlock session.staging_lock)
      in
      let image =
        if retain then
          Some
            (match pending_image with
            | Some image -> image
            | None -> Js_cmj_format.freeze cmj)
        else None
      in
      Mutex.lock session.staging_lock;
      Fun.protect
        (fun () ->
          Hashtbl.replace session.cmj_fingerprints destination
            {source = None; stats; fingerprint};
          Option.iter
            (fun image ->
              Hashtbl.replace session.published_cmjs name
                {
                  filename =
                    Compiler_request_state.canonical_output_path destination;
                  source = None;
                  stats;
                  fingerprint;
                  image;
                })
            image)
        ~finally:(fun () -> Mutex.unlock session.staging_lock))

type fingerprint_kind = Interface | Optimization

let published_fingerprint session ~kind ~filename =
  let entry =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        let table =
          match kind with
          | Interface -> session.cmi_fingerprints
          | Optimization -> session.cmj_fingerprints
        in
        Hashtbl.find_opt table filename)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  match entry with
  | None -> None
  | Some entry -> (
    try
      let checked = Option.value entry.source ~default:filename in
      if same_file_stats (Unix.stat checked) entry.stats then
        Some entry.fingerprint
      else None
    with Sys_error _ | Unix.Unix_error _ -> None)

let publish_session_ast session ~source =
  let staged =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        let staged = Hashtbl.find_opt session.staged_asts source in
        Hashtbl.remove session.staged_asts source;
        staged)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  Option.bind staged (validated_stage source)
  |> Option.iter (fun result ->
      let stats = Unix.stat source in
      Mutex.lock session.staging_lock;
      Fun.protect
        (fun () ->
          if Hashtbl.length session.published_asts >= 2048 then
            Hashtbl.clear session.published_asts;
          Hashtbl.replace session.published_asts source {stats; result})
        ~finally:(fun () -> Mutex.unlock session.staging_lock))

let publish_session_semantic session ~retain ~source ~destination =
  let staged =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () ->
        let staged = Hashtbl.find_opt session.staged_semantics source in
        remove_staged_semantic session source;
        staged)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  if retain then
    Option.bind staged (fun entry ->
        validated_stage source {stats = entry.stats; value = entry.value})
    |> Option.iter (fun value ->
        let stats = Unix.stat destination in
        if stats.Unix.st_size <= 4 * 1024 * 1024 then (
          Mutex.lock session.staging_lock;
          Fun.protect
            (fun () ->
              let previous =
                Hashtbl.find_opt session.published_semantics destination
              in
              Option.iter
                (fun (entry : published_semantic) ->
                  session.semantic_bytes <-
                    session.semantic_bytes - entry.stats.st_size)
                previous;
              session.semantic_generation <- session.semantic_generation + 1;
              Hashtbl.replace session.published_semantics destination
                {stats; generation = session.semantic_generation; value};
              session.semantic_bytes <- session.semantic_bytes + stats.st_size;
              while
                session.semantic_bytes > 16 * 1024 * 1024
                || Hashtbl.length session.published_semantics > 64
              do
                let oldest =
                  Hashtbl.fold
                    (fun path (entry : published_semantic) oldest ->
                      match oldest with
                      | Some (_, generation) when generation <= entry.generation
                        ->
                        oldest
                      | _ -> Some (path, entry.generation))
                    session.published_semantics None
                in
                match oldest with
                | None -> assert false
                | Some (path, _) ->
                  let entry = Hashtbl.find session.published_semantics path in
                  session.semantic_bytes <-
                    session.semantic_bytes - entry.stats.st_size;
                  Hashtbl.remove session.published_semantics path
              done)
            ~finally:(fun () -> Mutex.unlock session.staging_lock)))

let semantic_result session ~filename =
  let entry =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () -> Hashtbl.find_opt session.published_semantics filename)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  match entry with
  | None -> None
  | Some entry -> (
    try
      if same_file_stats (Unix.stat filename) entry.stats then
        Some
          (Marshal.from_bytes (Marshal.to_bytes entry.value []) 0
            : Cmt_format.cmt_infos)
      else None
    with Sys_error _ | Unix.Unix_error _ -> None)

let stage_module_result session ~input ~interface_source ~interface_file
    ~optimization_source ~optimization_file ~semantic_source ~dependencies
    ~generated_outputs =
  let interface_stats = Unix.stat interface_source in
  let interface_image =
    Env.published_compiled_cmi session.dependencies ~filename:interface_file
    |> Option.map snd
  in
  let interface_fingerprint =
    published_fingerprint session ~kind:Interface ~filename:interface_file
  in
  let optimization =
    match (optimization_source, optimization_file) with
    | Some source, Some filename -> (
      let stats = Unix.stat source in
      match published_fingerprint session ~kind:Optimization ~filename with
      | None -> None
      | Some fingerprint ->
        let name = unit_name_of_artifact filename in
        let image =
          Mutex.lock session.staging_lock;
          Fun.protect
            (fun () ->
              match Hashtbl.find_opt session.published_cmjs name with
              | Some entry
                when entry.fingerprint = fingerprint
                     && same_file_stats entry.stats stats ->
                Some entry.image
              | Some _ | None -> None)
            ~finally:(fun () -> Mutex.unlock session.staging_lock)
        in
        Some (filename, Some source, stats, fingerprint, image))
    | None, None | None, Some _ | Some _, None -> None
  in
  let semantic () =
    Option.bind semantic_source (fun source ->
        let staged =
          Mutex.lock session.staging_lock;
          Fun.protect
            (fun () -> Hashtbl.find_opt session.staged_semantics source)
            ~finally:(fun () -> Mutex.unlock session.staging_lock)
        in
        Option.bind staged (fun staged ->
            try
              if same_file_stats (Unix.stat source) staged.stats then
                Some
                  (Marshal.from_bytes (Marshal.to_bytes staged.value []) 0
                    : Cmt_format.cmt_infos)
              else None
            with Sys_error _ | Unix.Unix_error _ -> None))
  in
  Mutex.lock session.staging_lock;
  Fun.protect
    (fun () ->
      let diagnostics =
        Hashtbl.find_opt session.staged_diagnostics input
        |> Option.value ~default:[]
      in
      let captured_outputs =
        Hashtbl.find_opt session.staged_generated_outputs input
        |> Option.value ~default:[]
      in
      let result =
        {
          interface_file;
          interface_source = Some interface_source;
          interface_stats;
          interface_fingerprint;
          interface_image;
          optimization;
          semantic;
          diagnostics;
          dependencies;
          generated_outputs =
            List.sort_uniq String.compare (generated_outputs @ captured_outputs);
        }
      in
      Hashtbl.replace session.published_results interface_file result)
    ~finally:(fun () -> Mutex.unlock session.staging_lock)

let publish_module_result session ~input ~interface_file ~optimization_file
    ~semantic_file ~dependencies ~generated_outputs =
  let interface_stats = Unix.stat interface_file in
  let interface_image =
    Env.published_compiled_cmi session.dependencies ~filename:interface_file
    |> Option.map snd
  in
  Mutex.lock session.staging_lock;
  Fun.protect
    (fun () ->
      let valid_fingerprint (table : (string, published_fingerprint) Hashtbl.t)
          path stats =
        match Hashtbl.find_opt table path with
        | Some entry when same_file_stats entry.stats stats ->
          Some entry.fingerprint
        | _ -> None
      in
      let interface_fingerprint =
        valid_fingerprint session.cmi_fingerprints interface_file
          interface_stats
      in
      let optimization =
        Option.bind optimization_file (fun path ->
            try
              let stats = Unix.stat path in
              Option.map
                (fun fingerprint ->
                  let name = unit_name_of_artifact path in
                  let image =
                    match Hashtbl.find_opt session.published_cmjs name with
                    | Some entry
                      when same_file_stats entry.stats stats
                           && entry.fingerprint = fingerprint ->
                      Some entry.image
                    | _ -> None
                  in
                  (path, None, stats, fingerprint, image))
                (valid_fingerprint session.cmj_fingerprints path stats)
            with Sys_error _ | Unix.Unix_error _ -> None)
      in
      let semantic () =
        Option.bind semantic_file (fun filename ->
            semantic_result session ~filename)
      in
      let diagnostics =
        Hashtbl.find_opt session.staged_diagnostics input
        |> Option.value ~default:[]
      in
      Hashtbl.remove session.staged_diagnostics input;
      let generated_outputs =
        let captured =
          Hashtbl.find_opt session.staged_generated_outputs input
          |> Option.value ~default:[]
        in
        Hashtbl.remove session.staged_generated_outputs input;
        List.sort_uniq String.compare (generated_outputs @ captured)
        |> List.filter Sys.file_exists
      in
      let result =
        {
          interface_file;
          interface_source = None;
          interface_stats;
          interface_fingerprint;
          interface_image;
          optimization;
          semantic;
          diagnostics;
          dependencies;
          generated_outputs;
        }
      in
      Hashtbl.replace session.published_results interface_file result)
    ~finally:(fun () -> Mutex.unlock session.staging_lock)

let result_interface_is_current (result : module_result) =
  try
    same_file_stats
      (Unix.stat
         (Option.value result.interface_source ~default:result.interface_file))
      result.interface_stats
  with Sys_error _ | Unix.Unix_error _ -> false

let module_result session ~interface_file =
  let result =
    Mutex.lock session.staging_lock;
    Fun.protect
      (fun () -> Hashtbl.find_opt session.published_results interface_file)
      ~finally:(fun () -> Mutex.unlock session.staging_lock)
  in
  match result with
  | Some result when result_interface_is_current result -> Some result
  | Some _ | None -> None

let interface_fingerprint (result : module_result) =
  result.interface_fingerprint
let optimization_fingerprint (result : module_result) =
  Option.map (fun (_, _, _, fingerprint, _) -> fingerprint) result.optimization

let interface_signature (result : module_result) =
  if result_interface_is_current result then
    Option.map
      (fun image ->
        Frozen_values.copy_signature (Frozen_values.create_view image))
      result.interface_image
  else None

let optimization_metadata (result : module_result) =
  match result.optimization with
  | None -> None
  | Some (path, source, stats, _, image) -> (
    try
      if same_file_stats (Unix.stat (Option.value source ~default:path)) stats
      then Option.map Js_cmj_format.view image
      else None
    with Sys_error _ | Unix.Unix_error _ -> None)

let typed_semantic (result : module_result) = result.semantic ()

let result_diagnostics (result : module_result) = result.diagnostics
let result_dependencies (result : module_result) = result.dependencies
let result_generated_outputs (result : module_result) = result.generated_outputs

let run argv =
  let result = run_argv ~cwd:(Sys.getcwd ()) argv in
  prerr_string result.stderr;
  print_string result.stdout;
  result.exit_code
