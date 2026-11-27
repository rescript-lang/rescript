open OUnit2

let check condition message = assert_bool message condition

let parse arguments = Cli.parse (Array.of_list ("rescript" :: arguments))

let rejects arguments =
  try
    ignore (parse arguments);
    false
  with Cli.Parse_error _ -> true

let shows_help arguments =
  try
    ignore (parse arguments);
    false
  with Cli.Help -> true

let shows_version arguments =
  try
    ignore (parse arguments);
    false
  with Cli.Version -> true

let build_options arguments =
  match parse arguments with
  | Cli.Build options -> options
  | _ -> failwith "expected build command"

let watch_options arguments =
  match parse arguments with
  | Cli.Watch options -> options
  | _ -> failwith "expected watch command"

let tests =
  "cli_tests" >:: fun _context ->
  check
    (match parse [] with
    | Cli.Build _ -> true
    | _ -> false)
    "no subcommand defaults to build";
  check (rejects ["someFolder"]) "a bare folder is an unknown command";
  check
    ((build_options ["--prod"; "someFolder"]).folder = "someFolder")
    "a folder after an option uses the default build command";
  check
    ((build_options ["build"; "--"; "-v"]).folder = "-v")
    "double dash preserves an option-looking folder";
  check
    (rejects ["some-folder"; "--help"])
    "help does not hide an unknown command";
  check
    (rejects ["some-folder"; "-h"])
    "short help does not hide an unknown command";
  check
    (rejects ["some-folder"; "-V"])
    "version does not hide an unknown command";
  check (shows_help ["-h"]) "-h is short for --help";
  check (shows_version ["-V"]) "-V is short for --version";
  check (shows_help ["build"; "--help"]) "explicit build displays command help";
  check (shows_help ["build"; "-h"]) "explicit build accepts short command help";
  check (rejects ["-v"; "watch"]) "options before a command name are rejected";
  check (rejects ["--prod"; "build"]) "options before build are rejected";
  check
    ((build_options ["-v"; "./watch"]).folder = "./watch")
    "a folder named like a command can be built by its path";
  check
    ((build_options ["build"; "-vvvvv"]).verbosity = 5)
    "clustered verbosity after the command is counted";
  check
    (shows_help ["--help=plain"])
    "formatted root help remains a root display option";
  check
    (shows_help ["build"; "--help=groff"])
    "explicit Cmdliner help formats remain accepted";
  check
    (match parse ["build"; "-v"] with
    | Cli.Build _ -> true
    | _ -> false)
    "build accepts a trailing verbosity flag";
  check (shows_version ["--version"]) "the long global version flag is accepted";
  check
    (shows_version ["build"; "-V"])
    "explicit build accepts a trailing short version flag";
  check
    (shows_version ["build"; "--version"])
    "explicit build accepts the long trailing version flag";
  check
    (rejects ["watch"; "--no-timing"])
    "watch rejects build-only --no-timing";
  check
    ((build_options ["build"; "-n"; "."]).folder = "."
    && (build_options ["build"; "-n"; "."]).no_timing)
    "the short no-timing flag preserves the following folder";
  check
    ((build_options ["build"; "--no-timing"; "false"]).folder = "false"
    && (build_options ["build"; "--no-timing"; "false"]).no_timing)
    "no-timing does not interpret a folder named false as a boolean";
  check
    (rejects ["build"; "--no-timing=false"])
    "no-timing rejects explicit boolean values";
  check
    (not (build_options ["build"]).no_timing)
    "build defaults no-timing to false";
  check (build_options ["build"; "--prod"]).prod "build parses --prod";
  check (not (build_options ["build"]).prod) "build defaults --prod to false";
  check (watch_options ["watch"; "--prod"]).prod "watch parses --prod";
  check
    (match parse ["clean"; "--prod"] with
    | Cli.Clean {verbosity = 0; prod = true; folder = "."} -> true
    | _ -> false)
    "clean parses --prod";
  check
    (match parse ["clean"; "-q"] with
    | Cli.Clean {verbosity = -1; prod = false; folder = "."} -> true
    | _ -> false)
    "clean parses the quiet level";
  check (build_options ["--prod"]).prod
    "--prod selects the default build command";
  check
    ((build_options ["build"; "--features"; " native , web "]).features
    = Some ["native"; "web"])
    "feature names are trimmed";
  check
    ((build_options ["build"]).features = None)
    "build defaults features to none";
  check
    ((watch_options ["watch"; "--features"; "native"]).features
   = Some ["native"])
    "watch parses features";
  check
    ((build_options ["build"; "--features"; "native,web"]).features
   = (watch_options ["watch"; "--features"; "native,web"]).features)
    "build and watch use the same feature conversion";
  check (rejects ["build"; "--features"; ""]) "empty features are rejected";
  check
    (rejects [String.make 1 (Char.chr 0xff)])
    "non-UTF-8 arguments are rejected";
  check (watch_options ["watch"; "--clear-screen"]).clear_screen
    "watch parses --clear-screen";
  check
    (rejects ["build"; "--filter"; "["])
    "invalid filter regular expressions are rejected during CLI parsing";
  check
    (not (rejects ["build"; "--filter"; "Foo|Bar"]))
    "filter alternation is accepted";
  check
    (not (rejects ["build"; "--filter"; "(?:Foo|Bar)\\d+"]))
    "filter groups and shorthand classes are accepted";
  check
    (not (rejects ["build"; "--filter"; "\\QFoo.res\\E"]))
    "Perl quoting is accepted";
  check
    (rejects ["build"; "--filter"; "(Foo)\\1"])
    "backreferences, which Re cannot match, are rejected";
  check
    (rejects ["format"; "--stdin"; ".txt"])
    "format stdin validates the source extension";
  check
    (rejects ["format"; "input.res"; "--stdin"; ".res"])
    "format stdin conflicts with files regardless of argument order";
  check
    (rejects ["format"; "--stdin"; ".res"; "--check"])
    "format check conflicts with stdin regardless of argument order";
  check
    (match parse ["format"; "--stdin"; ".res"] with
    | Cli.Format (Cli.Format_stdin ".res") -> true
    | _ -> false)
    "format represents standard input independently from file inputs";
  check
    (match parse ["format"; "--check"; "A.res"] with
    | Cli.Format
        (Cli.Format_files {verbosity = 0; check = true; paths = ["A.res"]}) ->
      true
    | _ -> false)
    "format represents file inputs with their check mode";
  check (shows_help ["help"]) "the help command displays global help";
  check
    (shows_help ["help"; "build"])
    "the help command displays subcommand help";
  check (rejects ["help"; "unknown"]) "the help command rejects unknown topics";
  check (rejects ["compiler-args"]) "compiler-args requires a source path";
  check
    (rejects ["compiler-args"; "A.res"; "B.res"])
    "compiler-args rejects additional source paths";
  check
    (rejects ["build"; "--unknown-option"])
    "known subcommands reject unknown options"
