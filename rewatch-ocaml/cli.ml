type command =
  | Build of build_options
  | Clean of {verbosity: int; folder: string; prod: bool}
  | Watch of build_options
  | Format of format_input
  | Compiler_args of string

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
  | Format_files of {check: bool; paths: string list}

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
      value & flag_all
      & info ["q"; "quiet"] ~doc:"Decrease logging verbosity.")
  in
  Term.term_result
    (let+ verbose and+ quiet in
     match (verbose, quiet) with
     | _ :: _, _ :: _ ->
       Error (`Msg "--verbose cannot be used together with --quiet")
     | _ -> Ok (List.length verbose - List.length quiet))

let folder =
  Arg.(
    value
    & pos 0 string "."
    & info [] ~docv:"FOLDER"
        ~doc:"Path to the project or subproject containing rescript.json.")

let prod =
  Arg.(
    value & flag
    & info ["prod"] ~doc:"Skip development dependencies and sources.")

let features =
  let parse value =
    let values =
      String.split_on_char ',' value |> List.map String.trim
      |> List.filter (fun value -> value <> "")
    in
    if values = [] then
      Error
        (`Msg
          "--features must not be empty. Omit the flag to build with all features active.")
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
  Arg.(
    value
    & opt ~vopt:true bool false
    & info ["n"; "no-timing"] ~docv:"BOOL" ~doc:"Disable output timing.")

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
    (let+ _verbosity = verbosity and+ check and+ stdin and+ files in
     match (check, stdin, files) with
     | true, Some _, _ -> Error (`Msg "--stdin conflicts with --check")
     | _, Some _, _ :: _ -> Error (`Msg "files conflict with --stdin")
     | _, Some extension, [] -> Ok (Format (Format_stdin extension))
     | _, None, paths -> Ok (Format (Format_files {check; paths})))

let compiler_args_term =
  let path =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"PATH" ~doc:"ReScript source file (.res or .resi).")
  in
  let+ _verbosity = verbosity and+ path in
  Compiler_args path

let command_info name doc = Cmd.info name ~doc

let root =
  let build =
    Cmd.make (command_info "build" "Build the project.")
      (build_term ~watch:false)
  in
  let watch =
    Cmd.make (command_info "watch" "Build, then start a watcher.")
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
    Cmd.make (command_info "help" "Print this message or command help.")
      help_term
  in
  let info =
    let man =
      [
        `S "NOTES";
        `P
          "If no command is provided, the $(b,build) command is run by default. See $(b,rescript help build) for more information.";
        `P
          "To create a new ReScript project, or to add ReScript to an existing project, use https://github.com/rescript-lang/create-rescript-app.";
      ]
    in
    Cmd.info "rescript" ~version:("rescript " ^ Rewatch_version.version)
      ~doc:"Fast, Simple, Fully Typed JavaScript from the Future" ~man
  in
  Cmd.group info ~default:(build_term ~watch:false)
    [build; watch; clean; format; compiler_args; help]

type evaluation = Run of command | Exit of int

exception Parse_error of string
exception Help
exception Version

let argv_is_utf_8 argv = Array.for_all String.is_valid_utf_8 argv

(* Cmdliner owns option parsing. This adapter only reproduces clap's implicit
   build routing and global help/version placement before Cmdliner sees argv. *)
let normalize_argv argv =
  let is_short_global_cluster argument =
    let length = String.length argument in
    length > 1 && argument.[0] = '-' && argument.[1] <> '-'
    && String.for_all
         (function 'v' | 'q' | 'h' | 'V' -> true | _ -> false)
         (String.sub argument 1 (length - 1))
  in
  let short_cluster_contains character argument =
    is_short_global_cluster argument
    && String.contains_from argument 1 character
  in
  let is_verbosity = function
  | "-v" | "-vv" | "-vvv" | "-vvvv" | "--verbose" | "-q" | "-qq"
  | "-qqq" | "-qqqq" | "--quiet" -> true
  | argument ->
    is_short_global_cluster argument
    && not
         (short_cluster_contains 'h' argument
         || short_cluster_contains 'V' argument)
  in
  let is_help = function "-h" | "--help" -> true | _ -> false in
  let is_version = function "-V" | "--version" -> true | _ -> false in
  let is_global argument =
    is_short_global_cluster argument || is_verbosity argument
    || is_help argument || is_version argument
  in
  let display_request argument =
    if argument = "--help" then Some `Help
    else if argument = "--version" then Some `Version
    else if is_short_global_cluster argument then
      let rec first index =
        if index = String.length argument then None
        else
          match argument.[index] with
          | 'h' -> Some `Help
          | 'V' -> Some `Version
          | 'v' | 'q' -> first (index + 1)
          | _ -> None
      in
      first 1
    else None
  in
  let first_display_request arguments = List.find_map display_request arguments in
  let is_command = function
  | "build" | "watch" | "clean" | "format" | "compiler-args" | "help" ->
    true
  | _ -> false
  in
  let rec normalize_short_booleans = function
  | [] -> []
  | "--" :: rest -> "--" :: rest
  | ("-n" | "--no-timing") :: value :: rest
    when value <> "--"
         && (String.length value = 0 || value.[0] <> '-') ->
    ("--no-timing=" ^ value) :: normalize_short_booleans rest
  | ("-n" | "--no-timing") :: rest ->
    "--no-timing=true" :: normalize_short_booleans rest
  | "-n=true" :: rest -> "--no-timing=true" :: normalize_short_booleans rest
  | "-n=false" :: rest ->
    "--no-timing=false" :: normalize_short_booleans rest
  | argument :: rest -> argument :: normalize_short_booleans rest
  in
  let rec normalize_help = function
  | [] -> []
  | "--" :: rest -> "--" :: rest
  | ("-h" | "--help") :: rest -> "--help=plain" :: normalize_help rest
  | argument :: rest
    when is_short_global_cluster argument
         && short_cluster_contains 'h' argument
         &&
         let help_index = String.index_from argument 1 'h' in
         (not (short_cluster_contains 'V' argument))
         || help_index < String.index_from argument 1 'V' ->
    "--help=plain" :: normalize_help rest
  | argument :: rest -> argument :: normalize_help rest
  in
  let rec reject_subcommand_version = function
  | [] -> []
  | "--" :: rest -> "--" :: rest
  | "--version" :: rest -> "-V" :: reject_subcommand_version rest
  | argument :: rest -> argument :: reject_subcommand_version rest
  in
  let rec split_leading_globals globals = function
  | argument :: rest when is_global argument ->
    split_leading_globals (argument :: globals) rest
  | rest -> (List.rev globals, rest)
  in
  let before_double_dash arguments =
    let rec loop acc = function
    | [] | "--" :: _ -> List.rev acc
    | argument :: rest -> loop (argument :: acc) rest
    in
    loop [] arguments
  in
  let first_non_global arguments =
    before_double_dash arguments |> List.find_opt (fun arg -> not (is_global arg))
  in
  let partition_implicit arguments =
    let rec loop globals others = function
    | [] -> (List.rev globals, List.rev others)
    | "--" :: rest ->
      (List.rev globals, List.rev_append others ("--" :: rest))
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
      match first_non_global arguments with
      | Some command when is_command command ->
        let globals, command_and_rest = split_leading_globals [] arguments in
        (match first_display_request globals with
        | Some `Help -> [executable; "--help"]
        | Some `Version -> [executable; "--version"]
        | None ->
          (match command_and_rest with
          | command :: rest ->
            executable :: command
            :: reject_subcommand_version (globals @ rest)
          | [] -> assert false))
      | _ ->
        let globals, others = partition_implicit arguments in
        (match first_display_request globals with
        | Some `Help -> [executable; "--help"]
        | Some `Version -> [executable; "--version"]
        | None -> executable :: "build" :: (globals @ others))
    in
    Array.of_list
      (routed |> normalize_short_booleans |> normalize_help)

let eval argv =
  if not (argv_is_utf_8 argv) then (
    prerr_endline "invalid UTF-8 in command-line argument";
    Exit 2)
  else
    match Cmd.eval_value ~catch:false ~argv:(normalize_argv argv) root with
    | Ok (`Ok command) -> Run command
    | Ok `Help | Ok `Version -> Exit 0
    | Error _ -> Exit 2

let parse argv =
  if not (argv_is_utf_8 argv) then
    raise (Parse_error "invalid UTF-8 in command-line argument");
  let help_buffer = Buffer.create 256 in
  let error_buffer = Buffer.create 256 in
  let help = Stdlib.Format.formatter_of_buffer help_buffer in
  let err = Stdlib.Format.formatter_of_buffer error_buffer in
  let result =
    Cmd.eval_value ~catch:false ~help ~err ~argv:(normalize_argv argv) root
  in
  Stdlib.Format.pp_print_flush help ();
  Stdlib.Format.pp_print_flush err ();
  match result with
  | Ok (`Ok command) -> command
  | Ok `Help -> raise Help
  | Ok `Version -> raise Version
  | Error _ -> raise (Parse_error (Buffer.contents error_buffer))
