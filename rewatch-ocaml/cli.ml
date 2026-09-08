type command =
  | Build of build_options
  | Clean of {folder: string; prod: bool}
  | Watch of build_options
  | Format of {check: bool; stdin: string option; files: string list}
  | Compiler_args of string
  | Help of string option
  | Version

and build_options = {
  folder: string;
  prod: bool;
  features: string list option;
  warn_error: string option;
  after_build: string option;
  filter: string option;
  clear_screen: bool;
}

exception Error of string

let version = "13.0.0-alpha.6"

let usage =
  {|ReScript - Fast, Simple, Fully Typed JavaScript from the Future

Usage: rescript [OPTIONS] <COMMAND>

Commands:
  build          Build the project (default command)
  watch          Build, then start a watcher
  clean          Clean the build artifacts
  format         Format ReScript files
  compiler-args  Print compiler arguments for a ReScript source file
  help           Print this message or command help

Options:
  -v, --verbose...  Increase logging verbosity
  -q, --quiet...    Decrease logging verbosity
  -h, --help        Print help
  -V, --version     Print version|}

let command_usage = function
  | None -> usage
  | Some "build" ->
    "Usage: rescript build [OPTIONS] [FOLDER]\n\nOptions: --filter, --after-build, --warn-error, --features, --no-timing, --prod"
  | Some "watch" ->
    "Usage: rescript watch [OPTIONS] [FOLDER]\n\nOptions: --filter, --after-build, --warn-error, --features, --clear-screen, --prod"
  | Some "clean" -> "Usage: rescript clean [OPTIONS] [FOLDER]\n\nOptions: --prod"
  | Some "format" ->
    "Usage: rescript format [OPTIONS] [FILES]...\n\nOptions: --check, --stdin <.res|.resi>"
  | Some "compiler-args" -> "Usage: rescript compiler-args <PATH>"
  | Some command -> raise (Error ("unknown command " ^ command))

let option_before_double_dash names args =
  let rec loop = function
  | [] | "--" :: _ -> false
  | arg :: rest -> List.mem arg names || loop rest
  in
  loop args

let parse argv =
  let rec remove_leading_global_options = function
    | ("-v" | "-vv" | "-vvv" | "-vvvv" | "--verbose" | "-q" | "-qq"
      | "-qqq" | "-qqqq" | "--quiet")
      :: rest ->
      remove_leading_global_options rest
    | args -> args
  in
  let args =
    Array.to_list argv |> List.tl |> remove_leading_global_options
  in
  let parse_build ~watch ~explicit args =
    let parse_features value =
      let values =
        String.split_on_char ',' value |> List.map String.trim
        |> List.filter (fun x -> x <> "")
      in
      if values = [] then raise (Error "--features must not be empty");
      values
    in
    let parse_no_timing_value value =
      match value with
      | "true" | "false" -> ()
      | _ -> raise (Error ("invalid value for --no-timing: " ^ value))
    in
    let rec loop folder prod features warn_error after_build filter clear_screen
        positional_only = function
    | [] ->
      let command = {folder = Option.value folder ~default:"."; prod; features; warn_error; after_build; filter; clear_screen} in
      if watch then Watch command else Build command
    | "--" :: rest when not positional_only ->
      loop folder prod features warn_error after_build filter clear_screen true
        rest
    | ("-h" | "--help") :: _ when not positional_only ->
      Help (Some (if watch then "watch" else "build"))
    | ("-V" | "--version") :: _ when (not positional_only) && not explicit ->
      Version
    | "--prod" :: rest when not positional_only ->
      loop folder true features warn_error after_build filter clear_screen false
        rest
    | "--features" :: value :: rest when not positional_only ->
      loop folder prod (Some (parse_features value)) warn_error after_build
        filter clear_screen false rest
    | arg :: rest
      when (not positional_only) && String.starts_with ~prefix:"--features=" arg
      ->
      let value = String.sub arg 11 (String.length arg - 11) in
      loop folder prod (Some (parse_features value)) warn_error after_build
        filter clear_screen false rest
    | "--warn-error" :: value :: rest when not positional_only ->
      loop folder prod features (Some value) after_build filter clear_screen
        false rest
    | ("-a" | "--after-build") :: command :: rest when not positional_only ->
      loop folder prod features warn_error (Some command) filter clear_screen
        false rest
    | ("-f" | "--filter") :: pattern :: rest when not positional_only ->
      loop folder prod features warn_error after_build (Some pattern)
        clear_screen false rest
    | "--clear-screen" :: rest when watch && not positional_only ->
      loop folder prod features warn_error after_build filter true false rest
    | ("-n" | "--no-timing") :: _ when watch && not positional_only ->
      raise (Error "unknown option --no-timing")
    | arg :: _
      when watch && not positional_only
           && (String.starts_with ~prefix:"-n=" arg
              || String.starts_with ~prefix:"--no-timing=" arg) ->
      raise (Error "unknown option --no-timing")
    | ("-n" | "--no-timing") :: value :: rest
      when not positional_only && (value = "true" || value = "false") ->
      parse_no_timing_value value;
      loop folder prod features warn_error after_build filter clear_screen false
        rest
    | ("-n" | "--no-timing") :: rest when not positional_only ->
      loop folder prod features warn_error after_build filter clear_screen false
        rest
    | arg :: rest
      when (not positional_only)
           && (String.starts_with ~prefix:"-n=" arg
              || String.starts_with ~prefix:"--no-timing=" arg) ->
      let separator = String.index arg '=' in
      parse_no_timing_value
        (String.sub arg (separator + 1) (String.length arg - separator - 1));
      loop folder prod features warn_error after_build filter clear_screen false
        rest
    | ("-v" | "-vv" | "-vvv" | "-vvvv" | "--verbose" | "-q" | "-qq"
      | "-qqq" | "-qqqq" | "--quiet")
      :: rest
      when not positional_only ->
      loop folder prod features warn_error after_build filter clear_screen false
        rest
    | arg :: _
      when (not positional_only) && String.length arg > 0 && arg.[0] = '-' ->
      raise (Error ("unknown option " ^ arg))
    | arg :: rest -> (
      match folder with
      | None ->
        loop (Some arg) prod features warn_error after_build filter clear_screen
          positional_only rest
      | Some _ -> raise (Error "too many folder arguments"))
    in
    loop None false None None None None false false args
  in
  match args with
  | ["help"] | ["-h"] | ["--help"] -> Help None
  | ["help"; command] -> Help (Some command)
  | "compiler-args" :: ("-h" | "--help") :: _ ->
    Help (Some "compiler-args")
  | "compiler-args" :: [path] -> Compiler_args path
  | "compiler-args" :: _ -> raise (Error "compiler-args requires exactly one source file")
  | "format" :: rest ->
    let rec loop check stdin files = function
      | [] -> Format {check; stdin; files = List.rev files}
      | ("-h" | "--help") :: _ -> Help (Some "format")
      | ("-c" | "--check") :: more ->
        if Option.is_some stdin then
          raise (Error "--check conflicts with --stdin");
        loop true stdin files more
      | ("-s" | "--stdin") :: extension :: more ->
        if check then raise (Error "--stdin conflicts with --check");
        if files <> [] then raise (Error "--stdin conflicts with files");
        if extension <> ".res" && extension <> ".resi" then
          raise (Error "--stdin must be either .res or .resi");
        loop check (Some extension) files more
      | arg :: _ when String.length arg > 0 && arg.[0] = '-' -> raise (Error ("unknown format option " ^ arg))
      | file :: more ->
        if Option.is_some stdin then raise (Error "files conflict with --stdin");
        loop check stdin (file :: files) more
    in loop false None [] rest
  | "clean" :: rest ->
    let rec loop folder prod = function
      | [] -> Clean {folder = Option.value folder ~default:"."; prod}
      | ("-h" | "--help") :: _ -> Help (Some "clean")
      | "--prod" :: more -> loop folder true more
      | arg :: _ when String.length arg > 0 && arg.[0] = '-' -> raise (Error ("unknown clean option " ^ arg))
      | path :: more ->
        (match folder with
        | None -> loop (Some path) prod more
        | Some _ -> raise (Error "too many folder arguments"))
    in loop None false rest
  | "watch" :: rest -> parse_build ~watch:true ~explicit:true rest
  | "build" :: rest -> parse_build ~watch:false ~explicit:true rest
  | rest when option_before_double_dash ["-h"; "--help"] rest -> Help None
  | rest when option_before_double_dash ["-V"; "--version"] rest -> Version
  | rest -> parse_build ~watch:false ~explicit:false rest
