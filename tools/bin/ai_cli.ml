let log_and_exit = function
  | Ok log ->
    Printf.printf "%s\n" log;
    exit 0
  | Error log ->
    Printf.eprintf "%s\n" log;
    exit 1

let banner = "ReScript Assist"

let lint_check_help prog_name =
  Printf.sprintf
    {|%s

Run AI-oriented lint checks on a file or directory

Usage: %s lint check <FILE-OR-DIR> [--config <FILE>] [--json]

Notes:
- Text is the default output format
- Use --json for compact machine-readable JSON
- Both source AST and typed information are used when available

Example: %s lint check ./src/MyModule.res|}
    banner prog_name prog_name

let lint_help prog_name =
  Printf.sprintf
    {|%s

Run AI-oriented lint commands

Usage: %s lint <command>

Commands:

check <file-or-dir>                     Run AI-oriented lint checks
  [--config <file>]                       Use the given lint config file
  [--json]                                Output compact JSON

Example: %s lint check ./src/MyModule.res|}
    banner prog_name prog_name

let rewrite_run_help prog_name =
  Printf.sprintf
    {|%s

Rewrite a file or directory into a narrower agent-oriented source form

Usage: %s rewrite run <FILE-OR-DIR> [--config <FILE>] [--diff] [--json]

Notes:
- Files are rewritten in place
- Use --diff to preview the rewritten diff without modifying files
- Use --json for compact machine-readable output
- Rewrite rules are loaded from the same config discovery path as lint

Example: %s rewrite run ./src/MyModule.res|}
    banner prog_name prog_name

let rewrite_help prog_name =
  Printf.sprintf
    {|%s

Run AI-oriented rewrite commands

Usage: %s rewrite <command>

Commands:

run <file-or-dir>                       Rewrite source into agent-oriented normal form
  [--config <file>]                       Use the given lint config file
  [--diff]                                Preview the rewritten diff without writing files
  [--json]                                Output compact JSON

Example: %s rewrite run ./src/MyModule.res|}
    banner prog_name prog_name

let active_rules_help prog_name =
  Printf.sprintf
    {|%s

List lint and rewrite rules, whether they are currently active, and what they do

Usage: %s support active-rules <FILE-OR-DIR> [--config <FILE>] [--json]

Notes:
- Uses the same config discovery path as lint and rewrite
- Includes both lint and rewrite rules
- Text is the default output format

Example: %s support active-rules ./src|}
    banner prog_name prog_name

let show_help prog_name =
  Printf.sprintf
    {|%s

Show hover-style semantic information for a module, value, or type path

Usage: %s support show <SYMBOL-PATH> [--kind <auto|module|value|type>] [--context <FILE-OR-DIR>] [--comments <include|omit>]

Notes:
- Symbol paths are user-facing paths like String or String.localeCompare
- Context defaults to the current working directory
- Kind defaults to auto
- Comments default to include

Example: %s support show String.localeCompare|}
    banner prog_name prog_name

let find_references_help prog_name =
  Printf.sprintf
    {|%s

Find references for a symbol path or for the symbol at a source location

Usage:
  %s support find-references <SYMBOL-PATH> [--kind <auto|module|value|type>] [--context <FILE-OR-DIR>]
  %s support find-references --file <FILE> --line <LINE> --col <COL>

Notes:
- Positional input means symbol-path mode
- Location mode uses 1-based line and column numbers
- Context defaults to the current working directory in symbol-path mode

Example: %s support find-references String.localeCompare|}
    banner prog_name prog_name prog_name

let support_help prog_name =
  Printf.sprintf
    {|%s

Run AI-oriented support commands

Usage: %s support <command>

Commands:

active-rules <file-or-dir>              List lint/rewrite rules and whether they are active
  [--config <file>]                       Use the given lint config file
  [--json]                                Output compact JSON
show <symbol-path>                      Show hover-style semantic information
  [--kind <auto|module|value|type>]      Select what kind of symbol to resolve
  [--context <file-or-dir>]               Resolve within the package for this path
  [--comments <include|omit>]             Include or omit doc/comments in output
find-references <symbol-path>           Find references by symbol path
  [--kind <auto|module|value|type>]      Select what kind of symbol to resolve
  [--context <file-or-dir>]               Resolve within the package for this path
find-references --file <file>           Find references at a source location
  [--line <line>]                         Use 1-based line number
  [--col <col>]                           Use 1-based column number

Example: %s support active-rules ./src|}
    banner prog_name prog_name

let help prog_name =
  Printf.sprintf
    {|%s

Usage: %s <namespace> <command>

Namespaces:

lint
  check <file-or-dir>                     Run AI-oriented lint checks
rewrite
  run <file-or-dir>                       Rewrite source into agent-oriented normal form
support
  active-rules <file-or-dir>              List lint/rewrite rules and whether they are active
  show <symbol-path>                      Show hover-style semantic information
  find-references <symbol-path>           Find references by symbol path or source location
-v, --version                           Print version
-h, --help                              Print help|}
    banner prog_name

let top_level_commands =
  ["lint"; "rewrite"; "support"; "active-rules"; "show"; "find-references"]

let is_ai_command command = List.mem command top_level_commands

let moved_command_invocation command =
  match command with
  | "lint" -> "lint check <file-or-dir>"
  | "rewrite" -> "rewrite run <file-or-dir>"
  | "support" -> "support --help"
  | "active-rules" -> "support active-rules <file-or-dir>"
  | "show" -> "support show <symbol-path>"
  | "find-references" -> "support find-references <symbol-path>"
  | other -> other

let moved_command_message ~prog_name command =
  Printf.sprintf
    "error: `%s` has moved to the standalone `%s` binary.\n\nUse:\n  %s %s"
    command prog_name prog_name
    (moved_command_invocation command)

let namespace_command_message ~prog_name ~namespace ~command =
  Printf.sprintf "error: `%s` is now a namespace.\n\nUse:\n  %s %s %s" namespace
    prog_name namespace command

let run_lint_check ~prog_name = function
  | ["-h"] | ["--help"] -> log_and_exit (Ok (lint_check_help prog_name))
  | path :: args -> (
    let rec parse_args config_path json = function
      | [] -> Ok (config_path, json)
      | "--json" :: rest -> parse_args config_path true rest
      | "--config" :: config :: rest -> parse_args (Some config) json rest
      | _ -> Error (lint_check_help prog_name)
    in
    match parse_args None false args with
    | Error help -> log_and_exit (Error help)
    | Ok (config_path, json) -> (
      match Tools.Lint.run ?config_path ~json path with
      | Error err ->
        prerr_endline err;
        exit 2
      | Ok {Tools.Lint.output; has_findings} ->
        if output <> "" then print_endline output;
        exit (if has_findings then 1 else 0)))
  | _ -> log_and_exit (Error (lint_check_help prog_name))

let run_rewrite ~prog_name = function
  | ["-h"] | ["--help"] -> log_and_exit (Ok (rewrite_run_help prog_name))
  | path :: args -> (
    let rec parse_args config_path json diff = function
      | [] -> Ok (config_path, json, diff)
      | "--json" :: rest -> parse_args config_path true diff rest
      | "--diff" :: rest -> parse_args config_path json true rest
      | "--config" :: config :: rest -> parse_args (Some config) json diff rest
      | _ -> Error (rewrite_run_help prog_name)
    in
    match parse_args None false false args with
    | Error help -> log_and_exit (Error help)
    | Ok (config_path, json, diff) -> (
      let mode = if diff then Tools.Rewrite.Diff else Tools.Rewrite.Write in
      match Tools.Rewrite.run ?config_path ~json ~mode path with
      | Error err ->
        prerr_endline err;
        exit 2
      | Ok {Tools.Rewrite.output; _} ->
        if output <> "" then print_endline output;
        exit 0))
  | _ -> log_and_exit (Error (rewrite_run_help prog_name))

let run_active_rules ~prog_name = function
  | ["-h"] | ["--help"] -> log_and_exit (Ok (active_rules_help prog_name))
  | path :: args -> (
    let rec parse_args config_path json = function
      | [] -> Ok (config_path, json)
      | "--json" :: rest -> parse_args config_path true rest
      | "--config" :: config :: rest -> parse_args (Some config) json rest
      | _ -> Error (active_rules_help prog_name)
    in
    match parse_args None false args with
    | Error help -> log_and_exit (Error help)
    | Ok (config_path, json) -> (
      match Tools.ActiveRules.run ?config_path ~json path with
      | Error err ->
        prerr_endline err;
        exit 2
      | Ok {Tools.ActiveRules.output} ->
        if output <> "" then print_endline output;
        exit 0))
  | _ -> log_and_exit (Error (active_rules_help prog_name))

let run_show ~prog_name = function
  | ["-h"] | ["--help"] -> log_and_exit (Ok (show_help prog_name))
  | path :: args -> (
    let rec parse_args kind context_path comments_mode = function
      | [] -> Ok (kind, context_path, comments_mode)
      | "--kind" :: value :: rest -> (
        match Tools.Show.show_kind_of_string value with
        | Some kind -> parse_args kind context_path comments_mode rest
        | None -> Error (show_help prog_name))
      | "--comments" :: value :: rest -> (
        match Tools.Show.comments_mode_of_string value with
        | Some comments_mode -> parse_args kind context_path comments_mode rest
        | None -> Error (show_help prog_name))
      | "--context" :: path :: rest ->
        parse_args kind (Some path) comments_mode rest
      | _ -> Error (show_help prog_name)
    in
    match parse_args Tools.Show.Auto None Tools.Show.Include args with
    | Error help -> log_and_exit (Error help)
    | Ok (kind, context_path, comments_mode) -> (
      match Tools.Show.run ?context_path ~kind ~comments_mode path with
      | Error err ->
        prerr_endline err;
        exit 2
      | Ok {Tools.Show.output} ->
        if output <> "" then print_endline output;
        exit 0))
  | _ -> log_and_exit (Error (show_help prog_name))

let run_find_references ~prog_name = function
  | ["-h"] | ["--help"] -> log_and_exit (Ok (find_references_help prog_name))
  | args -> (
    let rec parse_args symbol_path kind context_path file_path line col =
      function
      | [] -> Ok (symbol_path, kind, context_path, file_path, line, col)
      | "--kind" :: value :: rest -> (
        match Tools.Find_references.symbol_kind_of_string value with
        | Some kind ->
          parse_args symbol_path kind context_path file_path line col rest
        | None -> Error (find_references_help prog_name))
      | "--context" :: path :: rest ->
        parse_args symbol_path kind (Some path) file_path line col rest
      | "--file" :: path :: rest ->
        parse_args symbol_path kind context_path (Some path) line col rest
      | "--line" :: value :: rest -> (
        match int_of_string_opt value with
        | Some line ->
          parse_args symbol_path kind context_path file_path (Some line) col
            rest
        | None -> Error (find_references_help prog_name))
      | "--col" :: value :: rest -> (
        match int_of_string_opt value with
        | Some col ->
          parse_args symbol_path kind context_path file_path line (Some col)
            rest
        | None -> Error (find_references_help prog_name))
      | value :: rest when not (String.starts_with ~prefix:"--" value) -> (
        match symbol_path with
        | None ->
          parse_args (Some value) kind context_path file_path line col rest
        | Some _ -> Error (find_references_help prog_name))
      | _ -> Error (find_references_help prog_name)
    in
    match
      parse_args None Tools.Find_references.Auto None None None None args
    with
    | Error help -> log_and_exit (Error help)
    | Ok (symbol_path, kind, context_path, file_path, line, col) -> (
      let query =
        match (symbol_path, file_path, line, col) with
        | Some symbol_path, None, None, None ->
          Some (Tools.Find_references.Symbol {symbol_path; kind; context_path})
        | None, Some file_path, Some line, Some col ->
          Some (Tools.Find_references.Location {file_path; line; col})
        | _ -> None
      in
      match query with
      | None -> log_and_exit (Error (find_references_help prog_name))
      | Some query -> (
        match Tools.Find_references.run query with
        | Error err ->
          prerr_endline err;
          exit 2
        | Ok {Tools.Find_references.output; _} ->
          if output <> "" then print_endline output;
          exit 0)))

let lint_namespace_help_or_redirect ~prog_name = function
  | [] | ["-h"] | ["--help"] -> log_and_exit (Ok (lint_help prog_name))
  | "check" :: rest -> run_lint_check ~prog_name rest
  | _ ->
    log_and_exit
      (Error
         (namespace_command_message ~prog_name ~namespace:"lint"
            ~command:"check <file-or-dir>"))

let rewrite_namespace_help_or_redirect ~prog_name = function
  | [] | ["-h"] | ["--help"] -> log_and_exit (Ok (rewrite_help prog_name))
  | "run" :: rest -> run_rewrite ~prog_name rest
  | _ ->
    log_and_exit
      (Error
         (namespace_command_message ~prog_name ~namespace:"rewrite"
            ~command:"run <file-or-dir>"))

let support_namespace_help_or_redirect ~prog_name = function
  | [] | ["-h"] | ["--help"] -> log_and_exit (Ok (support_help prog_name))
  | "active-rules" :: rest -> run_active_rules ~prog_name rest
  | "show" :: rest -> run_show ~prog_name rest
  | "find-references" :: rest -> run_find_references ~prog_name rest
  | _ -> log_and_exit (Error (support_help prog_name))

let main ~prog_name ~version () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "lint" :: rest -> lint_namespace_help_or_redirect ~prog_name rest
  | "rewrite" :: rest -> rewrite_namespace_help_or_redirect ~prog_name rest
  | "support" :: rest -> support_namespace_help_or_redirect ~prog_name rest
  | (("active-rules" | "show" | "find-references") as command) :: _ ->
    log_and_exit
      (Error
         (Printf.sprintf
            "error: `%s` is now under the `support` namespace.\n\nUse:\n  %s %s"
            command prog_name
            (moved_command_invocation command)))
  | ["-h"] | ["--help"] -> log_and_exit (Ok (help prog_name))
  | ["-v"] | ["--version"] -> log_and_exit (Ok version)
  | _ -> log_and_exit (Error (help prog_name))
