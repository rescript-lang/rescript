let check condition message = if not condition then failwith message

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

let () =
  check
    (match parse [] with Cli.Build _ -> true | _ -> false)
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
  check (shows_help ["some-folder"; "--help"])
    "implicit command folder help uses global help";
  check (shows_help ["some-folder"; "-h"])
    "short implicit command help uses global help";
  check (shows_help ["build"; "--help"])
    "explicit build displays command help";
  check (shows_help ["build"; "-h"])
    "explicit build accepts short command help";
  check
    (match parse ["-vvvv"; "watch"] with Cli.Watch _ -> true | _ -> false)
    "leading verbosity before watch";
  check
    (match parse ["build"; "-v"] with Cli.Build _ -> true | _ -> false)
    "build accepts a trailing verbosity flag";
  check (shows_version ["-V"; "build"])
    "a leading short version flag has global precedence";
  check (shows_version ["some-folder"; "-V"])
    "implicit build extracts a trailing global version flag";
  check (shows_version ["--version"])
    "the long global version flag is accepted";
  check (rejects ["build"; "-V"])
    "explicit build rejects a trailing global version flag";
  check (rejects ["watch"; "--no-timing"])
    "watch rejects build-only --no-timing";
  check
    ((build_options ["build"; "-n=false"; "."]).folder = ".")
    "build accepts short no-timing boolean values";
  check (build_options ["build"; "--prod"]).prod
    "build parses --prod";
  check (not (build_options ["build"]).prod)
    "build defaults --prod to false";
  check (watch_options ["watch"; "--prod"]).prod
    "watch parses --prod";
  check
    (match parse ["clean"; "--prod"] with
    | Cli.Clean {prod = true; folder = "."} -> true
    | _ -> false)
    "clean parses --prod";
  check (build_options ["--prod"]).prod
    "--prod selects the implicit build command";
  check
    ((build_options ["build"; "--features"; " native , web "]).features
    = Some ["native"; "web"])
    "feature names are trimmed";
  check ((build_options ["build"]).features = None)
    "build defaults features to none";
  check
    ((watch_options ["watch"; "--features"; "native"]).features
    = Some ["native"])
    "watch parses features";
  check
    ((build_options ["build"; "--features"; "native,web"]).features
    = (watch_options ["watch"; "--features"; "native,web"]).features)
    "build and watch use the same feature conversion";
  check (rejects ["build"; "--features"; ""])
    "empty features are rejected";
  check (rejects [String.make 1 (Char.chr 0xff)])
    "non-UTF-8 arguments are rejected";
  check (watch_options ["watch"; "--clear-screen"]).clear_screen
    "watch parses --clear-screen";
  check (rejects ["build"; "--filter"; "["])
    "invalid filter regular expressions are rejected during CLI parsing";
  check (rejects ["format"; "--stdin"; ".txt"])
    "format stdin validates the source extension";
  check (rejects ["format"; "input.res"; "--stdin"; ".res"])
    "format stdin conflicts with files regardless of argument order";
  check (rejects ["format"; "--stdin"; ".res"; "--check"])
    "format check conflicts with stdin regardless of argument order";
  check (shows_help ["help"])
    "the help command displays global help";
  check (shows_help ["help"; "build"])
    "the help command displays subcommand help";
  check (rejects ["help"; "unknown"])
    "the help command rejects unknown topics"
