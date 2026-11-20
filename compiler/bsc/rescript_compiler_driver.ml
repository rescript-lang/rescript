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

let absname = Compile_request.absname
exception Request_exit of int

module Error_message_utils_support = struct
  external to_comment : Res_comment.t -> Error_message_utils.Parser.comment
    = "%identity"
  external from_comment : Error_message_utils.Parser.comment -> Res_comment.t
    = "%identity"

  (* The parser hooks are installed once, before worker domains start. *)
  let install_parser () =
    (Error_message_utils.Parser.parse_source :=
       fun source ->
         let res =
           Res_driver.parse_implementation_from_source
             ~display_filename:"<none>" ~source
         in
         (* Hints rewrite source text extracted at a location. Text that does
            not parse cleanly (e.g. a location that leaves out the parentheses
            around an operand) must not be turned into a suggestion. *)
         if res.invalid then ([], [])
         else (res.parsetree, res.comments |> List.map to_comment));

    Error_message_utils.Parser.reprint_source :=
      fun parsetree comments ->
        Res_printer.print_implementation parsetree
          ~comments:(comments |> List.map from_comment)
          ~width:80

  let setup () =
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

let process_file sourcefile ppf =
  (* The input name must identify the source when writing the binary AST. *)
  setup_outcome_printer ();
  Error_message_utils_support.setup ();
  let kind =
    Ext_file_extensions.classify_input
      (Ext_filename.get_extension_maybe sourcefile)
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
      |> Cmd_ppx_apply.apply_rewriters ~tool_name:Js_config.tool_name Ml
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
      |> Cmd_ppx_apply.apply_rewriters ~tool_name:Js_config.tool_name Mli
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
  | None -> Compiler_request_output.write_stdout_binary formatted
  | Some fname -> Ext_io.write_file fname formatted

(* The temporary file is made absolute: a relative temporary directory names a
   directory under the process's working directory, while a request resolves
   relative paths against its own. *)
let eval (s : string) ~suffix =
  let tmpfile = Filename.temp_file "eval" suffix in
  let tmpfile =
    if Filename.is_relative tmpfile then Filename.concat (Sys.getcwd ()) tmpfile
    else tmpfile
  in
  Fun.protect
    ~finally:(fun () ->
      if not !((Clflags.current ()).verbose) then
        try Sys.remove tmpfile with _ -> ())
    (fun () ->
      Ext_io.write_file tmpfile s;
      anonymous ~rev_args:[tmpfile])

(* let (//) = Filename.concat *)

let bs_version_string = "ReScript " ^ Bs_version.version

let print_version_string () =
  Compiler_request_output.print_stdout bs_version_string;
  raise_notrace (Request_exit 0)

type flag_error =
  | Unknown_flag of string
  | Missing_argument of string
  | Not_a_flag of string
  | Reserved_flag of string

(* Walks [flags] as the parser does, without applying them: some flags print,
   exit or compile a file. *)
let check_flags ~reserved flags =
  let rec check = function
    | [] -> Ok ()
    | flag :: rest -> (
      if flag = "" || flag.[0] <> '-' then Error (Not_a_flag flag)
      else if List.mem flag reserved then Error (Reserved_flag flag)
      else
        match Compile_request.takes_value flag with
        | None -> Error (Unknown_flag flag)
        | Some false -> check rest
        | Some true -> (
          match rest with
          | [] -> Error (Missing_argument flag)
          | _value :: rest -> check rest))
  in
  check flags

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
    match Compile_request.of_argv (Array.to_list args) with
    | Ok {request; commands = []} -> (
      try Compile_request.apply request
      with Bsc_args.Bad _ ->
        Location.prerr_warning pexp_loc (Preprocessor "invalid flags for bsc"))
    | Ok _ | Error _ ->
      Location.prerr_warning pexp_loc (Preprocessor "invalid flags for bsc"))
  | Some e -> Location.raise_errorf ~loc:e.pexp_loc "string array expected"

(* These shared hooks are installed when the driver module loads, before any
   request can start on another domain. Concurrent Lazy.force calls would race
   on a first request. *)
let () =
  Bs_conditional_initial.setup_env ();
  setup_outcome_printer ();
  Error_message_utils_support.install_parser ();
  let flags = "flags" in
  Ast_config.add_structure flags file_level_flags_handler;
  Ast_config.add_signature flags file_level_flags_handler;
  Ident.capture_request_baseline ()

type result = {exit_code: int; stdout: string; stderr: string}

let build_identity = Rescript_compiler_build_identity.value

(* Request_local gives every request the state of a new compiler process.
   On top of that, a request needs bsc's own defaults, and allocates the shared
   predefined type graphs in a fixed order: lazy first use on different worker
   domains would otherwise shift serialized type IDs. *)
let start_request () =
  Clflags.reset ();
  Predef.reset_for_request ();
  Env.reset_initial_for_request ()

(* Interfaces that this domain keeps for later requests must not keep this
   request's memoized abbreviations, so they are reset while the request's
   memo list is still current. A failed request may also have stopped with
   some of their types marked, so the domain drops them. *)
let leave_cached_interfaces_clean action =
  match action () with
  | result ->
    Btype.cleanup_abbrev ();
    if result.exit_code <> 0 then Bs_cmi_load.discard_cached_cmis ();
    result
  | exception exn ->
    Btype.cleanup_abbrev ();
    Bs_cmi_load.discard_cached_cmis ();
    raise exn

let with_fresh_request_states ~cwd action =
  Predef.with_fresh (fun () ->
      Env.with_fresh_initial (fun () ->
          Ident.with_fresh (fun () ->
              Request_local.with_fresh (fun () ->
                  Compiler_request_state.with_fresh ~cwd (fun () ->
                      leave_cached_interfaces_clean action)))))

let usage_text () =
  Bsc_args.usage_string ~usage (Compile_request.usage_entries ())

let run_command (command : Compile_request.command) =
  match command with
  | Eval source -> eval source ~suffix:Literals.suffix_res
  | Print_version -> print_version_string ()
  | Warn_help ->
    Warnings.help_warnings ();
    raise_notrace (Request_exit 0)
  | Format file -> format_file file
  | Reprint_source file -> reprint_source_file file
  | Help -> raise_notrace (Bsc_args.Help (usage_text ()))

let run_command_line argv =
  let argv = Array.to_list argv |> List.tl in
  match Compile_request.of_argv argv with
  | Error error ->
    let message =
      match error with
      | Unknown_flag flag ->
        "Unknown option \"" ^ flag ^ "\".\n" ^ usage_text ()
      | Missing_value flag ->
        "Option \"" ^ flag ^ "\" needs an argument.\n" ^ usage_text ()
      | Invalid_flag_value {message} -> message
    in
    Bsc_args.bad_arg message
  | Ok {request; commands; arguments} ->
    if List.mem Compile_request.Help commands then run_command Help;
    Compile_request.apply request;
    List.iter run_command commands;
    anonymous ~rev_args:(List.rev arguments)

let run_argv ?run_external ~capture ~cwd argv =
  with_fresh_request_states ~cwd (fun () ->
      start_request ();
      let execute () =
        try
          run_command_line argv;
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
              (* Written past the formatter, which would count the PPX's
                 output as the current column and indent later errors. *)
              Format.pp_print_flush (ppf ()) ();
              Compiler_request_output.write_stderr stderr;
              status)
            action
      in
      if capture then
        let exit_code, stdout, stderr =
          Compiler_request_output.with_capture (fun () ->
              Misc.Color.set_color_tag_handling
                (Compiler_request_output.stdout_formatter ());
              Misc.Color.set_color_tag_handling
                (Compiler_request_output.stderr_formatter ());
              run_with_external_owner execute)
        in
        {exit_code; stdout; stderr}
      else
        (* A bsc process streams to its own stdout and stderr. *)
        let exit_code =
          Fun.protect
            (fun () -> run_with_external_owner execute)
            ~finally:(fun () ->
              Format.pp_print_flush Format.std_formatter ();
              Format.pp_print_flush Format.err_formatter ();
              flush stdout;
              flush stderr)
        in
        {exit_code; stdout = ""; stderr = ""})

let run_request ~run_external ~cwd ~argv ~input =
  let logical_argv = Array.of_list ("bsc" :: (argv @ [input])) in
  run_argv ?run_external ~capture:true ~cwd logical_argv

let run argv = (run_argv ~capture:false ~cwd:(Sys.getcwd ()) argv).exit_code
