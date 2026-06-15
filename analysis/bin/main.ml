open Analysis

let help =
  {|
**Private CLI For rescript-vscode usage only**

API examples:
  ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 currentContent.res true
  ./rescript-editor-analysis.exe definition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe documentSymbol src/Foo.res
  ./rescript-editor-analysis.exe hover src/MyFile.res 10 2 true
  ./rescript-editor-analysis.exe references src/MyFile.res 10 2
  ./rescript-editor-analysis.exe prepareRename src/MyFile.res 10 2
  ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo
  ./rescript-editor-analysis.exe diagnosticSyntax src/MyFile.res
  ./rescript-editor-analysis.exe inlayHint src/MyFile.res 0 3 25
  ./rescript-editor-analysis.exe codeLens src/MyFile.res

Dev-time examples:
  ./rescript-editor-analysis.exe dump src/MyFile.res src/MyFile2.res
  ./rescript-editor-analysis.exe test src/MyFile.res

Note: positions are zero-indexed (start at 0 0), following LSP.
https://microsoft.github.io/language-server-protocol/specification#position

Options:
  completion: compute autocomplete for MyFile.res at line 0 and column 4,
    where MyFile.res is being edited and the editor content is in file current.res.

    ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 current.res

  definition: get definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe definition src/MyFile.res 10 2

  typeDefinition: get type definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 10 2

  documentSymbol: get all symbols declared in MyFile.res

    ./rescript-editor-analysis.exe documentSymbol src/MyFile.res

  hover: get inferred type for MyFile.res at line 10 column 2 (supporting markdown links):

    ./rescript-editor-analysis.exe hover src/MyFile.res 10 2 true

  references: get all references to item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe references src/MyFile.res 10 2

  prepareRename: quickly compute the identifier range (and placeholder) for item at line 10 column 2 without scanning references:

    ./rescript-editor-analysis.exe prepareRename src/MyFile.res 10 2

  rename: rename all appearances of item in MyFile.res at line 10 column 2 with foo:

    ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo

  semanticTokens: return token semantic highlighting info for MyFile.res

    ./rescript-editor-analysis.exe semanticTokens src/MyFile.res

  createInterface: print to stdout the interface file for src/MyFile.res

    ./rescript-editor-analysis.exe createInterface src/MyFile.res lib/bs/src/MyFile.cmi

  format: print to stdout the formatted version of the provided file

    ./rescript-editor-analysis.exe format src/MyFile.res

  diagnosticSyntax: print to stdout diagnostic for syntax

    ./rescript-editor-analysis.exe diagnosticSyntax src/MyFile.res

  inlayHint: get all inlay Hint between line 0 and 3 declared in MyFile.res. Last argument is maximum of character length for inlay hints

    ./rescript-editor-analysis.exe inlayHint src/MyFile.res 0 3 25

  codeLens: get all code lens entries for file src/MyFile.res

    ./rescript-editor-analysis.exe codeLens src/MyFile.res

  signatureHelp: get signature help if available for position at line 10 column 2 in src/MyFile.res

    ./rescript-editor-analysis.exe signatureHelp src/MyFile.res 10 2

  test: run tests specified by special comments in file src/MyFile.res

    ./rescript-editor-analysis.exe test src/src/MyFile.res
|}

let main () =
  let state = Shared_types.create_state () in
  let args = Array.to_list Sys.argv in
  let debug_level, args =
    match args with
    | _ :: "debug-dump" :: log_level :: rest ->
      ( (match log_level with
        | "verbose" -> Debug.Verbose
        | "regular" -> Regular
        | _ -> Off),
        "dummy" :: rest )
    | args -> (Off, args)
  in
  Debug.debug_level := debug_level;
  let debug = debug_level <> Debug.Off in
  let print_header_info path line col =
    if debug then
      Printf.printf "Debug level: %s\n%s:%s-%s\n\n"
        (match debug_level with
        | Debug.Verbose -> "verbose"
        | Regular -> "regular"
        | Off -> "off")
        path line col
  in
  match args with
  | [_; "cache-project"; root_path] -> (
    Cfg.read_project_config_cache := false;
    let uri = Uri.from_path root_path in
    match Packages.get_package ~state ~uri with
    | Some package -> Cache.cache_project package
    | None -> print_endline "\"ERR\"")
  | [_; "cache-delete"; root_path] -> (
    Cfg.read_project_config_cache := false;
    let uri = Uri.from_path root_path in
    match Packages.find_root ~uri (Hashtbl.create 0) with
    | Some (`Bs root_path) -> (
      match Build_system.get_lib_bs root_path with
      | None -> print_endline "\"ERR\""
      | Some lib_bs ->
        Cache.delete_cache (Cache.target_file_from_lib_bs lib_bs);
        print_endline "\"OK\"")
    | _ -> print_endline "\"ERR: Did not find root \"")
  | [_; "completion"; path; line; col; current_file] ->
    print_header_info path line col;
    Cli.completion ~debug ~path
      ~pos:(int_of_string line, int_of_string col)
      ~current_file ~state
  | [_; "completionResolve"; path; module_path] ->
    Cli.completion_resolve ~state ~path ~module_path
  | [_; "definition"; path; line; col] ->
    Cli.definition ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug
  | [_; "typeDefinition"; path; line; col] ->
    Cli.type_definition ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug
  | [_; "documentSymbol"; path] -> Cli.document_symbol ~path
  | [_; "hover"; path; line; col; current_file; supports_markdown_links] ->
    Cli.hover ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~current_file ~debug
      ~supports_markdown_links:
        (match supports_markdown_links with
        | "true" -> true
        | _ -> false)
  | [
   _;
   "signatureHelp";
   path;
   line;
   col;
   current_file;
   allow_for_constructor_payloads;
  ] ->
    Cli.signature_help ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~current_file ~debug
      ~allow_for_constructor_payloads:
        (match allow_for_constructor_payloads with
        | "true" -> true
        | _ -> false)
  | [_; "inlayHint"; path; line_start; line_end; max_length] ->
    let max_length =
      try Some (int_of_string max_length) with Failure _ -> None
    in
    Cli.inlayhint ~state ~path
      ~pos:(int_of_string line_start, int_of_string line_end)
      ~max_length ~debug
  | [_; "codeLens"; path] -> Cli.code_lens ~state ~path ~debug
  | [
   _; "codeAction"; path; start_line; start_col; end_line; end_col; current_file;
  ] ->
    Cli.code_action ~state ~path
      ~start_pos:(int_of_string start_line, int_of_string start_col)
      ~end_pos:(int_of_string end_line, int_of_string end_col)
      ~current_file ~debug
  | [_; "codemod"; path; line; col; typ; hint] ->
    let typ =
      match typ with
      | "add-missing-cases" -> Codemod.AddMissingCases
      | _ -> raise (Failure "unsupported type")
    in
    let source = Files.read_file path |> Option.value ~default:"" in
    `String
      (Codemod.transform ~source
         ~pos:(int_of_string line, int_of_string col)
         ~debug ~typ ~hint)
    |> Yojson.Safe.pretty_to_string ~std:true
    |> print_endline
  | [_; "diagnosticSyntax"; path] -> Cli.diagnostic_syntax ~path
  | [_; "references"; path; line; col] ->
    Cli.references ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug
  | [_; "prepareRename"; path; line; col] ->
    Cli.prepare_rename ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug
  | [_; "rename"; path; line; col; new_name] ->
    Cli.rename ~state ~path
      ~pos:(int_of_string line, int_of_string col)
      ~new_name ~debug
  | [_; "semanticTokens"; current_file] ->
    Cli.semantic_tokens ~path:current_file
  | [_; "createInterface"; path; cmi_file] ->
    Cli.create_interface ~path ~cmi_file
  | [_; "format"; path] -> Cli.format ~path
  | [_; "test"; path] -> Cli.test ~state ~path
  | [_; "cmt"; rescript_json; cmt_path] ->
    Cmt_viewer.dump ~state rescript_json cmt_path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
