let docHelp =
  {|ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE>

Example: rescript-tools doc ./path/to/EntryPointLib.res|}

let formatCodeblocksHelp =
  {|ReScript Tools

Format ReScript code blocks in docstrings or markdown files

Usage: rescript-tools format-codeblocks <FILE> [--stdout] [--transform-assert-equal]

Example: rescript-tools format-codeblocks ./path/to/MyModule.res|}

let extractCodeblocksHelp =
  {|ReScript Tools

Extract ReScript code blocks from docstrings or markdown files

Usage: rescript-tools extract-codeblocks <FILE> [--transform-assert-equal]

Example: rescript-tools extract-codeblocks ./path/to/MyModule.res|}

let lintHelp =
  {|ReScript Tools

Run AI-oriented lint checks on a file or directory

Usage: rescript-tools lint <FILE-OR-DIR> [--config <FILE>] [--json]

Notes:
- Text is the default output format
- Use --json for compact machine-readable JSON
- Both source AST and typed information are used when available

Example: rescript-tools lint ./src/MyModule.res|}

let rewriteHelp =
  {|ReScript Tools

Rewrite a file or directory into a narrower agent-oriented source form

Usage: rescript-tools rewrite <FILE-OR-DIR> [--config <FILE>] [--diff] [--json]

Notes:
- Files are rewritten in place
- Use --diff to preview the rewritten diff without modifying files
- Use --json for compact machine-readable output
- Rewrite rules are loaded from the same config discovery path as lint

Example: rescript-tools rewrite ./src/MyModule.res|}

let activeRulesHelp =
  {|ReScript Tools

List lint and rewrite rules, whether they are currently active, and what they do

Usage: rescript-tools active-rules <FILE-OR-DIR> [--config <FILE>] [--json]

Notes:
- Uses the same config discovery path as lint and rewrite
- Includes both lint and rewrite rules
- Text is the default output format

Example: rescript-tools active-rules ./src|}

let showHelp =
  {|ReScript Tools

Show hover-style semantic information for a module, value, or type path

Usage: rescript-tools show <SYMBOL-PATH> [--kind <auto|module|value|type>] [--context <FILE-OR-DIR>] [--comments <include|omit>]

Notes:
- Symbol paths are user-facing paths like String or String.localeCompare
- Context defaults to the current working directory
- Kind defaults to auto
- Comments default to include

Example: rescript-tools show String.localeCompare|}

let findReferencesHelp =
  {|ReScript Tools

Find references for a symbol path or for the symbol at a source location

Usage:
  rescript-tools find-references <SYMBOL-PATH> [--kind <auto|module|value|type>] [--context <FILE-OR-DIR>]
  rescript-tools find-references --file <FILE> --line <LINE> --col <COL>

Notes:
- Positional input means symbol-path mode
- Location mode uses 1-based line and column numbers
- Context defaults to the current working directory in symbol-path mode

Example: rescript-tools find-references String.localeCompare|}

let help =
  {|ReScript Tools

Usage: rescript-tools [command]

Commands:

migrate <file> [--stdout]               Runs the migration tool on the given file
migrate-all <root>                      Runs migrations for all project sources under <root>
doc <file>                              Generate documentation
format-codeblocks <file>                Format ReScript code blocks
  [--stdout]                              Output to stdout
  [--transform-assert-equal]              Transform `assertEqual` to `==`
extract-codeblocks <file>               Extract ReScript code blocks from file
  [--transform-assert-equal]              Transform `==` to `assertEqual`
lint <file-or-dir>                      Run AI-oriented lint checks
  [--config <file>]                       Use the given lint config file
  [--json]                                Output compact JSON
rewrite <file-or-dir>                   Rewrite source into agent-oriented normal form
  [--config <file>]                       Use the given lint config file
  [--diff]                                Preview the rewritten diff without writing files
  [--json]                                Output compact JSON
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
reanalyze                               Reanalyze
reanalyze-server                        Start reanalyze server
-v, --version                           Print version
-h, --help                              Print help|}

let logAndExit = function
  | Ok log ->
    Printf.printf "%s\n" log;
    exit 0
  | Error log ->
    Printf.eprintf "%s\n" log;
    exit 1

let version = Version.version

let main () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "doc" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok docHelp)
    | [path] ->
      (* NOTE: Internal use to generate docs from compiler *)
      let () =
        match Sys.getenv_opt "FROM_COMPILER" with
        | Some "true" -> Analysis.Cfg.isDocGenFromCompiler := true
        | _ -> ()
      in
      logAndExit (Tools.extractDocs ~entryPointFile:path ~debug:false)
    | _ -> logAndExit (Error docHelp))
  | "migrate" :: file :: opts -> (
    let isStdout = List.mem "--stdout" opts in
    let outputMode = if isStdout then `Stdout else `File in
    match
      (Tools.Migrate.migrate ~entryPointFile:file ~outputMode, outputMode)
    with
    | Ok content, `Stdout -> print_endline content
    | result, `File -> logAndExit result
    | Error e, _ -> logAndExit (Error e))
  | "migrate-all" :: root :: _opts -> (
    let rootPath =
      if Filename.is_relative root then Unix.realpath root else root
    in
    match Analysis.Packages.newBsPackage ~rootPath with
    | None ->
      logAndExit
        (Error
           (Printf.sprintf
              "error: failed to load ReScript project at %s (missing \
               rescript.json?)"
              rootPath))
    | Some package ->
      let moduleNames =
        Analysis.SharedTypes.FileSet.elements package.projectFiles
      in
      let files =
        moduleNames
        |> List.filter_map (fun modName ->
               Hashtbl.find_opt package.pathsForModule modName
               |> Option.map Analysis.SharedTypes.getSrc)
        |> List.concat
        |> List.filter (fun path ->
               Filename.check_suffix path ".res"
               || Filename.check_suffix path ".resi")
      in
      let total = List.length files in
      if total = 0 then logAndExit (Ok "No source files found to migrate")
      else
        let process_one file =
          (file, Tools.Migrate.migrate ~entryPointFile:file ~outputMode:`File)
        in
        let results = List.map process_one files in
        let migrated, unchanged, failures =
          results
          |> List.fold_left
               (fun (migrated, unchanged, failures) (file, res) ->
                 match res with
                 | Ok msg ->
                   let base = Filename.basename file in
                   if msg = base ^ ": File migrated successfully" then
                     (migrated + 1, unchanged, failures)
                   else if msg = base ^ ": File did not need migration" then
                     (migrated, unchanged + 1, failures)
                   else
                     (* Unknown OK message, count as unchanged *)
                     (migrated, unchanged + 1, failures)
                 | Error _ -> (migrated, unchanged, failures + 1))
               (0, 0, 0)
        in
        let summary =
          Printf.sprintf
            "Migration summary: migrated %d, unchanged %d, failed %d, total %d"
            migrated unchanged failures total
        in
        if failures > 0 then logAndExit (Error summary)
        else logAndExit (Ok summary))
  | "format-codeblocks" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok formatCodeblocksHelp)
    | path :: args -> (
      let isStdout = List.mem "--stdout" args in
      let transformAssertEqual = List.mem "--transform-assert-equal" args in
      let outputMode = if isStdout then `Stdout else `File in
      Clflags.color := Some Misc.Color.Never;
      match
        ( Tools.FormatCodeblocks.formatCodeBlocksInFile ~outputMode
            ~transformAssertEqual ~entryPointFile:path,
          outputMode )
      with
      | Ok content, `Stdout -> print_endline content
      | result, `File -> logAndExit result
      | Error e, _ -> logAndExit (Error e))
    | _ -> logAndExit (Error formatCodeblocksHelp))
  | "extract-codeblocks" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok extractCodeblocksHelp)
    | path :: args -> (
      let transformAssertEqual = List.mem "--transform-assert-equal" args in
      Clflags.color := Some Misc.Color.Never;

      (* TODO: Add result/JSON mode *)
      match
        Tools.ExtractCodeblocks.extractCodeblocksFromFile ~transformAssertEqual
          ~entryPointFile:path
      with
      | Ok _ as r ->
        print_endline (Analysis.Protocol.stringifyResult r);
        exit 0
      | Error _ as r ->
        print_endline (Analysis.Protocol.stringifyResult r);
        exit 1)
    | _ -> logAndExit (Error extractCodeblocksHelp))
  | "lint" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok lintHelp)
    | path :: args -> (
      let rec parse_args config_path json = function
        | [] -> Ok (config_path, json)
        | "--json" :: rest -> parse_args config_path true rest
        | "--config" :: config :: rest -> parse_args (Some config) json rest
        | _ -> Error lintHelp
      in
      match parse_args None false args with
      | Error help -> logAndExit (Error help)
      | Ok (config_path, json) -> (
        match Tools.Lint.run ?config_path ~json path with
        | Error err ->
          prerr_endline err;
          exit 2
        | Ok {Tools.Lint.output; has_findings} ->
          if output <> "" then print_endline output;
          exit (if has_findings then 1 else 0)))
    | _ -> logAndExit (Error lintHelp))
  | "rewrite" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok rewriteHelp)
    | path :: args -> (
      let rec parse_args config_path json diff = function
        | [] -> Ok (config_path, json, diff)
        | "--json" :: rest -> parse_args config_path true diff rest
        | "--diff" :: rest -> parse_args config_path json true rest
        | "--config" :: config :: rest ->
          parse_args (Some config) json diff rest
        | _ -> Error rewriteHelp
      in
      match parse_args None false false args with
      | Error help -> logAndExit (Error help)
      | Ok (config_path, json, diff) -> (
        let mode = if diff then Tools.Rewrite.Diff else Tools.Rewrite.Write in
        match Tools.Rewrite.run ?config_path ~json ~mode path with
        | Error err ->
          prerr_endline err;
          exit 2
        | Ok {Tools.Rewrite.output; _} ->
          if output <> "" then print_endline output;
          exit 0))
    | _ -> logAndExit (Error rewriteHelp))
  | "active-rules" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok activeRulesHelp)
    | path :: args -> (
      let rec parse_args config_path json = function
        | [] -> Ok (config_path, json)
        | "--json" :: rest -> parse_args config_path true rest
        | "--config" :: config :: rest -> parse_args (Some config) json rest
        | _ -> Error activeRulesHelp
      in
      match parse_args None false args with
      | Error help -> logAndExit (Error help)
      | Ok (config_path, json) -> (
        match Tools.ActiveRules.run ?config_path ~json path with
        | Error err ->
          prerr_endline err;
          exit 2
        | Ok {Tools.ActiveRules.output} ->
          if output <> "" then print_endline output;
          exit 0))
    | _ -> logAndExit (Error activeRulesHelp))
  | "show" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok showHelp)
    | path :: args -> (
      let rec parse_args kind context_path comments_mode = function
        | [] -> Ok (kind, context_path, comments_mode)
        | "--kind" :: value :: rest -> (
          match Tools.Show.show_kind_of_string value with
          | Some kind -> parse_args kind context_path comments_mode rest
          | None -> Error showHelp)
        | "--comments" :: value :: rest -> (
          match Tools.Show.comments_mode_of_string value with
          | Some comments_mode -> parse_args kind context_path comments_mode rest
          | None -> Error showHelp)
        | "--context" :: path :: rest ->
          parse_args kind (Some path) comments_mode rest
        | _ -> Error showHelp
      in
      match parse_args Tools.Show.Auto None Tools.Show.Include args with
      | Error help -> logAndExit (Error help)
      | Ok (kind, context_path, comments_mode) -> (
        match Tools.Show.run ?context_path ~kind ~comments_mode path with
        | Error err ->
          prerr_endline err;
          exit 2
        | Ok {Tools.Show.output} ->
          if output <> "" then print_endline output;
          exit 0))
    | _ -> logAndExit (Error showHelp))
  | "find-references" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok findReferencesHelp)
    | args ->
      let rec parse_args symbol_path kind context_path file_path line col =
          function
        | [] -> Ok (symbol_path, kind, context_path, file_path, line, col)
        | "--kind" :: value :: rest -> (
          match Tools.Find_references.symbol_kind_of_string value with
          | Some kind ->
            parse_args symbol_path kind context_path file_path line col rest
          | None -> Error findReferencesHelp)
        | "--context" :: path :: rest ->
          parse_args symbol_path kind (Some path) file_path line col rest
        | "--file" :: path :: rest ->
          parse_args symbol_path kind context_path (Some path) line col rest
        | "--line" :: value :: rest -> (
          match int_of_string_opt value with
          | Some line ->
            parse_args symbol_path kind context_path file_path (Some line) col
              rest
          | None -> Error findReferencesHelp)
        | "--col" :: value :: rest -> (
          match int_of_string_opt value with
          | Some col ->
            parse_args symbol_path kind context_path file_path line (Some col)
              rest
          | None -> Error findReferencesHelp)
        | value :: rest when not (String.starts_with ~prefix:"--" value) -> (
          match symbol_path with
          | None ->
            parse_args (Some value) kind context_path file_path line col rest
          | Some _ -> Error findReferencesHelp)
        | _ -> Error findReferencesHelp
      in
      match parse_args None Tools.Find_references.Auto None None None None args with
      | Error help -> logAndExit (Error help)
      | Ok (symbol_path, kind, context_path, file_path, line, col) -> (
        let query =
          match (symbol_path, file_path, line, col) with
          | Some symbol_path, None, None, None ->
            Some
              (Tools.Find_references.Symbol {symbol_path; kind; context_path})
          | None, Some file_path, Some line, Some col ->
            Some (Tools.Find_references.Location {file_path; line; col})
          | _ -> None
        in
        match query with
        | None -> logAndExit (Error findReferencesHelp)
        | Some query -> (
          match Tools.Find_references.run query with
          | Error err ->
            prerr_endline err;
            exit 2
          | Ok {Tools.Find_references.output; _} ->
            if output <> "" then print_endline output;
            exit 0)))
  | "reanalyze" :: _ ->
    if Sys.getenv_opt "RESCRIPT_REANALYZE_NO_SERVER" = Some "1" then (
      let len = Array.length Sys.argv in
      for i = 1 to len - 2 do
        Sys.argv.(i) <- Sys.argv.(i + 1)
      done;
      Sys.argv.(len - 1) <- "";
      Reanalyze.cli ())
    else
      (* Transparent delegation is supported only for the editor invocation:
         `reanalyze -json` (and nothing else). *)
      let argv_for_server =
        let args = Array.to_list Sys.argv in
        let rest =
          match args with
          | _ :: "reanalyze" :: rest -> rest
          | _ :: rest -> rest
          | [] -> []
        in
        rest |> List.filter (fun s -> s <> "") |> Array.of_list
      in
      if argv_for_server = [|"-json"|] then (
        match Reanalyze.ReanalyzeServer.try_request_default () with
        | Some resp ->
          output_string stdout resp.stdout;
          output_string stderr resp.stderr;
          flush stdout;
          flush stderr;
          exit resp.exit_code
        | None ->
          let len = Array.length Sys.argv in
          for i = 1 to len - 2 do
            Sys.argv.(i) <- Sys.argv.(i + 1)
          done;
          Sys.argv.(len - 1) <- "";
          Reanalyze.cli ())
      else
        let len = Array.length Sys.argv in
        for i = 1 to len - 2 do
          Sys.argv.(i) <- Sys.argv.(i + 1)
        done;
        Sys.argv.(len - 1) <- "";
        Reanalyze.cli ()
  | "reanalyze-server" :: _ ->
    let len = Array.length Sys.argv in
    for i = 1 to len - 2 do
      Sys.argv.(i) <- Sys.argv.(i + 1)
    done;
    Sys.argv.(len - 1) <- "";
    Reanalyze.ReanalyzeServer.server_cli ~parse_argv:Reanalyze.parse_argv
      ~run_analysis:Reanalyze.runAnalysis ()
  | "extract-embedded" :: extPointNames :: filename :: _ ->
    logAndExit
      (Ok
         (Tools.extractEmbedded
            ~extensionPoints:(extPointNames |> String.split_on_char ',')
            ~filename))
  | ["ppx"; file_in; file_out] ->
    let ic = open_in_bin file_in in
    let magic =
      really_input_string ic (String.length Config.ast_impl_magic_number)
    in
    let loc = input_value ic in
    let ast0 : Parsetree0.structure = input_value ic in
    let prefix =
      match ast0 with
      | c1 :: c2 :: _ -> [c1; c2]
      | _ -> []
    in
    let ast = prefix @ ast0 in
    close_in ic;
    let oc = open_out_bin file_out in
    output_string oc magic;
    output_value oc loc;
    output_value oc ast;
    close_out oc;
    exit 0
  | ["-h"] | ["--help"] -> logAndExit (Ok help)
  | ["-v"] | ["--version"] -> logAndExit (Ok version)
  | _ -> logAndExit (Error help)

let () = main ()
