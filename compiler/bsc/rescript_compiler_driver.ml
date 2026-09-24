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
             ~ignore_parse_errors:!((Clflags.current ()).ignore_parse_errors))
        ppf sourcefile
    | Resi ->
      let sourcefile = set_abs_input_name sourcefile in
      Js_implementation.interface
        ~parser:
          (Res_driver.parse_interface
             ~ignore_parse_errors:!((Clflags.current ()).ignore_parse_errors))
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

type result = {exit_code: int; stdout: string; stderr: string}

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

let run_argv ?run_external ~cwd argv =
  with_fresh_request_states ~cwd (fun () ->
      reset_state ~new_request:true ();
      Cmt_format.set_args argv;
      let execute () =
        try
          Bsc_args.parse_exn ~argv (command_line_flags ()) anonymous ~usage;
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
      let exit_code, stdout, stderr =
        Fun.protect
          (fun () ->
            Compiler_request_output.with_capture (fun () ->
                Misc.Color.set_color_tag_handling
                  (Compiler_request_output.stdout_formatter ());
                Misc.Color.set_color_tag_handling
                  (Compiler_request_output.stderr_formatter ());
                run_with_external_owner execute))
          ~finally:reset_state
      in
      {exit_code; stdout; stderr})

let run_request ~run_external ~cwd ~argv ~input =
  let logical_argv = Array.of_list ("bsc" :: (argv @ [input])) in
  run_argv ?run_external ~cwd logical_argv

let run argv =
  let result = run_argv ~cwd:(Sys.getcwd ()) argv in
  prerr_string result.stderr;
  print_string result.stdout;
  result.exit_code
