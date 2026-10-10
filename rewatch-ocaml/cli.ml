type command =
  | Build of build_options
  | Clean of {verbosity: int; folder: string; prod: bool}
  | Watch of build_options
  | Format of format_input
  | Compiler_args of {verbosity: int; path: string}

and build_options = {
  verbosity: int;
  folder: string;
  prod: bool;
  features: string list option;
  warn_error: string option;
  after_build: string option;
  filter: Source_filter.t option;
  clear_screen: bool;
  no_timing: bool;
}

and format_input =
  | Format_stdin of string
  | Format_files of {verbosity: int; check: bool; paths: string list}

open Cmdliner
open Cmdliner.Term.Syntax

let verbosity =
  let verbose =
    Arg.(
      value & flag_all
      & info ["v"; "verbose"] ~doc:"Increase logging verbosity.")
  in
  let quiet =
    Arg.(
      value & flag_all & info ["q"; "quiet"] ~doc:"Decrease logging verbosity.")
  in
  Term.term_result
    (let+ verbose and+ quiet in
     match (verbose, quiet) with
     | _ :: _, _ :: _ ->
       Error (`Msg "--verbose cannot be used together with --quiet")
     | _ -> Ok (List.length verbose - List.length quiet))

let folder =
  Arg.(
    value & pos 0 string "."
    & info [] ~docv:"FOLDER"
        ~doc:"Path to the project or subproject containing rescript.json.")

let prod =
  Arg.(
    value & flag
    & info ["prod"] ~doc:"Skip development dependencies and sources.")

let features =
  let parse value =
    let values =
      String.split_on_char ',' value
      |> List.map String.trim
      |> List.filter (fun value -> value <> "")
    in
    if values = [] then
      Error
        (`Msg
           "--features must not be empty. Omit the flag to build with all \
            features active.")
    else Ok values
  in
  let print formatter values =
    Stdlib.Format.pp_print_string formatter (String.concat "," values)
  in
  let converter = Arg.conv (parse, print) in
  Arg.(
    value
    & opt (some converter) None
    & info ["features"] ~docv:"FEATURES"
        ~doc:"Restrict the current package to comma-separated features.")

let warn_error =
  Arg.(
    value
    & opt (some string) None
    & info ["warn-error"] ~docv:"WARNINGS"
        ~doc:"Override warning configuration from rescript.json.")

let after_build =
  Arg.(
    value
    & opt (some string) None
    & info ["a"; "after-build"] ~docv:"COMMAND"
        ~doc:"Run an additional command after a successful build.")

let filter =
  let parse value =
    match Source_filter.compile value with
    | Ok filter -> Ok filter
    | Error message -> Error (`Msg message)
  in
  let print formatter filter =
    Stdlib.Format.pp_print_string formatter (Source_filter.pattern filter)
  in
  Arg.(
    value
    & opt (some (conv (parse, print))) None
    & info ["f"; "filter"] ~docv:"REGEX"
        ~doc:"Filter source files by regular expression.")

let no_timing =
  Arg.(value & flag & info ["n"; "no-timing"] ~doc:"Disable output timing.")

let clear_screen =
  Arg.(
    value & flag
    & info ["clear-screen"]
        ~doc:"Clear the terminal before each interactive rebuild.")

let build_term ~watch =
  let no_timing = if watch then Term.const false else no_timing in
  let clear_screen = if watch then clear_screen else Term.const false in
  let+ verbosity
  and+ folder
  and+ prod
  and+ features
  and+ warn_error
  and+ after_build
  and+ filter
  and+ no_timing
  and+ clear_screen in
  let options : build_options =
    {
      verbosity;
      folder;
      prod;
      features;
      warn_error;
      after_build;
      filter;
      clear_screen;
      no_timing;
    }
  in
  if watch then Watch options else Build options

let clean_term =
  let+ verbosity and+ folder and+ prod in
  Clean {verbosity; folder; prod}

let format_term =
  let extension = Arg.enum [(".res", ".res"); (".resi", ".resi")] in
  let stdin =
    Arg.(
      value
      & opt (some extension) None
      & info ["s"; "stdin"] ~docv:"EXTENSION"
          ~doc:"Read stdin and write formatted source to stdout.")
  in
  let check =
    Arg.(
      value & flag
      & info ["c"; "check"] ~doc:"Check formatting without modifying files.")
  in
  let files = Arg.(value & pos_all string [] & info [] ~docv:"FILES") in
  Term.term_result
    (let+ verbosity = verbosity and+ check and+ stdin and+ files in
     match (check, stdin, files) with
     | true, Some _, _ -> Error (`Msg "--stdin conflicts with --check")
     | _, Some _, _ :: _ -> Error (`Msg "files conflict with --stdin")
     | _, Some extension, [] -> Ok (Format (Format_stdin extension))
     | _, None, paths -> Ok (Format (Format_files {verbosity; check; paths})))

let compiler_args_term =
  let path =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"PATH" ~doc:"ReScript source file (.res or .resi).")
  in
  let+ verbosity = verbosity and+ path in
  Compiler_args {verbosity; path}

let exits =
  [
    Cmd.Exit.info 0 ~doc:"on success.";
    Cmd.Exit.info 1 ~doc:"on build, configuration, or file system errors.";
    Cmd.Exit.info 2
      ~doc:"on command-line usage errors and invalid package dependencies.";
    Cmd.Exit.info 129 ~max:143
      ~doc:"when interrupted by a signal (128 plus the signal number).";
  ]

let command_info name doc = Cmd.info name ~doc ~exits

let root =
  let build =
    Cmd.make
      (command_info "build" "Build the project.")
      (build_term ~watch:false)
  in
  let watch =
    Cmd.make
      (command_info "watch" "Build, then start a watcher.")
      (build_term ~watch:true)
  in
  let clean =
    Cmd.make (command_info "clean" "Clean build artifacts.") clean_term
  in
  let format =
    Cmd.make (command_info "format" "Format ReScript files.") format_term
  in
  let compiler_args =
    Cmd.make
      (command_info "compiler-args"
         "Print compiler arguments for a ReScript source file.")
      compiler_args_term
  in
  let help =
    let topic =
      Arg.(value & pos 0 (some string) None & info [] ~docv:"COMMAND")
    in
    let help_term =
      Term.ret
        (let+ commands = Term.choice_names and+ topic in
         match topic with
         | None -> `Help (`Plain, None)
         | Some command when List.mem command commands ->
           `Help (`Plain, Some command)
         | Some command ->
           `Error (false, Printf.sprintf "unknown command %S" command))
    in
    Cmd.make
      (command_info "help" "Print this message or command help.")
      help_term
  in
  let info =
    let man =
      [
        `S "NOTES";
        `P
          "If no command is provided, the $(b,build) command is run by \
           default. See $(b,rescript help build) for more information.";
        `P
          "To create a new ReScript project, or to add ReScript to an existing \
           project, use https://github.com/rescript-lang/create-rescript-app.";
      ]
    in
    Cmd.info "rescript" ~exits
      ~version:("rescript " ^ Rewatch_version.version)
      ~doc:"Fast, Simple, Fully Typed JavaScript from the Future" ~man
  in
  Cmd.group info ~default:(build_term ~watch:false)
    [build; watch; clean; format; compiler_args; help]

type evaluation = Run of command | Exit of int

exception Parse_error of string
exception Help
exception Version

(* Bare project folders must select the default build command, even though
   Cmdliner otherwise treats them as unknown commands. Move global options
   behind the selected command and expand short help/version clusters because
   Cmdliner's standard display options only provide long names. A root display
   request takes precedence over otherwise invalid implicit-build arguments. *)
let normalize_argv argv =
  let is_short_global_cluster argument =
    let length = String.length argument in
    length > 1
    && argument.[0] = '-'
    && argument.[1] <> '-'
    && String.for_all
         (function
           | 'v' | 'q' | 'h' | 'V' -> true
           | _ -> false)
         (String.sub argument 1 (length - 1))
  in
  let short_cluster_contains character argument =
    is_short_global_cluster argument
    && String.contains_from argument 1 character
  in
  let is_global = function
    | "--verbose" | "--quiet" | "--help" | "--version" -> true
    | argument ->
      is_short_global_cluster argument
      || String.starts_with ~prefix:"--help=" argument
  in
  let requests_help argument =
    argument = "--help"
    || String.starts_with ~prefix:"--help=" argument
    || short_cluster_contains 'h' argument
  in
  let requests_version argument =
    argument = "--version" || short_cluster_contains 'V' argument
  in
  let is_command = function
    | "build" | "watch" | "clean" | "format" | "compiler-args" | "help" -> true
    | _ -> false
  in
  let rec normalize_display_options = function
    | [] -> []
    | "--" :: rest -> "--" :: rest
    | argument :: rest when String.starts_with ~prefix:"--help=" argument ->
      argument :: normalize_display_options rest
    | argument :: rest when requests_help argument ->
      "--help=plain" :: normalize_display_options rest
    | argument :: rest when requests_version argument ->
      "--version" :: normalize_display_options rest
    | argument :: rest -> argument :: normalize_display_options rest
  in
  let explicit_command arguments =
    let rec loop globals = function
      | [] | "--" :: _ -> None
      | argument :: rest when is_global argument ->
        loop (argument :: globals) rest
      | command :: rest when is_command command ->
        Some (List.rev globals, command, rest)
      | _ -> None
    in
    loop [] arguments
  in
  let partition_implicit arguments =
    let rec loop globals others = function
      | [] -> (List.rev globals, List.rev others)
      | "--" :: rest -> (List.rev globals, List.rev_append others ("--" :: rest))
      | argument :: rest when is_global argument ->
        loop (argument :: globals) others rest
      | argument :: rest -> loop globals (argument :: others) rest
    in
    loop [] [] arguments
  in
  match Array.to_list argv with
  | [] -> argv
  | executable :: arguments ->
    let routed =
      match explicit_command arguments with
      | Some (globals, command, rest) ->
        executable :: command :: (globals @ rest)
      | None ->
        let globals, others = partition_implicit arguments in
        if
          List.exists
            (fun argument ->
              requests_help argument || requests_version argument)
            globals
        then executable :: globals
        else executable :: "build" :: (globals @ others)
    in
    Array.of_list (normalize_display_options routed)

(* Evaluates the command line and returns Cmdliner's result with the
   diagnostics it wrote, or [Error] for arguments that are not UTF-8. Help is
   written to [help] when given, otherwise to stdout. *)
let evaluate ?help argv =
  if not (Array.for_all String.is_valid_utf_8 argv) then
    Error "invalid UTF-8 in command-line argument"
  else
    let error_buffer = Buffer.create 256 in
    let err = Stdlib.Format.formatter_of_buffer error_buffer in
    let result =
      Cmd.eval_value ~catch:false ?help ~err ~argv:(normalize_argv argv) root
    in
    Option.iter (fun help -> Stdlib.Format.pp_print_flush help ()) help;
    Stdlib.Format.pp_print_flush err ();
    Ok (result, Buffer.contents error_buffer)

let eval argv =
  match evaluate argv with
  | Error message ->
    prerr_endline message;
    Exit 2
  | Ok (result, errors) -> (
    (* Cmdliner styles its diagnostics whenever TERM allows it, even when stderr
       is redirected, and offers no way to override that choice. Capture them
       and apply the color policy used for all other output. *)
    prerr_string
      (if Output.colors_enabled ~interactive:(Unix.isatty Unix.stderr) then
         errors
       else Output.strip_sgr errors);
    flush stderr;
    match result with
    | Ok (`Ok command) -> Run command
    | Ok `Help | Ok `Version -> Exit 0
    | Error _ -> Exit 2)

let parse argv =
  let help = Stdlib.Format.formatter_of_buffer (Buffer.create 256) in
  match evaluate ~help argv with
  | Error message -> raise (Parse_error message)
  | Ok (Ok (`Ok command), _) -> command
  | Ok (Ok `Help, _) -> raise Help
  | Ok (Ok `Version, _) -> raise Version
  | Ok (Error _, errors) -> raise (Parse_error errors)
