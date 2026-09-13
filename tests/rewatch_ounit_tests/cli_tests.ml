open OUnit2

let check condition message = assert_bool message condition

let parse arguments = Cli.parse (Array.of_list ("rescript-ocaml" :: arguments))

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
  check
    ((build_options ["someFolder"]).folder = "someFolder")
    "a bare folder uses the implicit build command";
  check
    ((build_options ["my-project"; "-v"]).folder = "my-project")
    "a trailing global verbosity flag keeps the implicit folder";
  check
    ((build_options ["--"; "-v"]).folder = "-v")
    "double dash preserves an option-looking folder";
  check
    (shows_help ["some-folder"; "--help"])
    "implicit command folder help uses global help";
  check
    (shows_help ["some-folder"; "-h"])
    "short implicit command help uses global help";
  check (shows_help ["build"; "--help"]) "explicit build displays command help";
  check (shows_help ["build"; "-h"]) "explicit build accepts short command help";
  check
    (match parse ["-vvvv"; "watch"] with
    | Cli.Watch _ -> true
    | _ -> false)
    "leading verbosity before watch";
  check
    ((build_options ["-vvvvv"; "build"]).verbosity = 5)
    "arbitrarily long clustered verbosity remains global";
  check
    (shows_version ["-vV"; "build"])
    "a clustered leading version flag has global precedence";
  check
    (shows_help ["-Vh"; "build"])
    "help takes precedence over version in a leading cluster";
  check
    (shows_help ["-hV"; "build"])
    "a leading help flag wins when it precedes version in a cluster";
  check
    (shows_help ["build"; "-hV"])
    "subcommand help takes precedence over version";
  check
    (shows_help ["build"; "-Vh"])
    "subcommand help precedence is independent of cluster order";
  check
    (shows_help ["--version"; "build"; "--help"])
    "subcommand help takes precedence over a leading version option";
  check
    (shows_help ["--version"; "build"; "--help=plain"])
    "formatted subcommand help takes precedence over a leading version option";
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
  check
    (rejects ["-v"; "-q"])
    "implicit build rejects conflicting verbose and quiet modes";
  check
    (rejects ["build"; "--verbose"; "--quiet"])
    "explicit build rejects conflicting verbose and quiet modes";
  check
    (shows_version ["-V"; "build"])
    "a leading short version flag has global precedence";
  check
    (shows_version ["some-folder"; "-V"])
    "implicit build extracts a trailing global version flag";
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
    (match parse ["-q"; "clean"] with
    | Cli.Clean {verbosity = -1; prod = false; folder = "."} -> true
    | _ -> false)
    "clean retains the global quiet level";
  check (build_options ["--prod"]).prod
    "--prod selects the implicit build command";
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
    "Rust-style filter alternation is accepted";
  check
    (not (rejects ["build"; "--filter"; "(?:Foo|Bar)\\d+"]))
    "Rust-style filter groups and shorthand classes are accepted";
  check
    (rejects ["build"; "--filter"; "[a-z&&[^aeiou]]"])
    "class set operations are rejected instead of changing meaning";
  check
    (rejects ["build"; "--filter"; "\\QFoo.res\\E"])
    "Perl quoting unsupported by Rust is rejected";
  check (rejects ["build"; "--filter"; "\\Z"]) "Perl-only anchors are rejected";
  check
    (rejects ["build"; "--filter"; "\\e"])
    "Perl-only character escapes are rejected";
  check
    (rejects ["build"; "--filter"; "\\o{123}"])
    "braced octal escapes unsupported by Rust are rejected";
  check
    (rejects ["build"; "--filter"; "[z-a]"])
    "descending character ranges unsupported by Rust are rejected";
  check
    (rejects ["build"; "--filter"; "[a-\\d]"])
    "character-class range endpoints unsupported by Rust are rejected";
  check
    (rejects ["build"; "--filter"; "[\\d-z]"])
    "escaped character-class range starts unsupported by Rust are rejected";
  check
    (rejects ["build"; "--filter"; "^[\\W]+\\.res$"])
    "shorthand classes with divergent in-class semantics are rejected";
  check
    (rejects ["build"; "--filter"; "[[.a.]]"])
    "collating elements with divergent class semantics are rejected";
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
    | Cli.Format (Cli.Format_files {check = true; paths = ["A.res"]}) -> true
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
